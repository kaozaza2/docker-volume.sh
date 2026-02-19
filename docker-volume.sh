#!/usr/bin/env bash
# docker-volume-toolbox — Swiss-army knife for Docker named volumes.
# Requires: docker (daemon must be running)
set -euo pipefail
IFS=$'\n\t'

readonly SCRIPT_NAME="$(basename "$0")"
readonly DOCKER_IMAGE="docker-volume-toolbox:latest"
readonly TOOL_DOCKERFILE='FROM alpine:latest
RUN apk add --no-cache coreutils tar zip xz findutils shadow'

# ─── output helpers ───────────────────────────────────────────────────────────

_err() { printf 'error: %s\n' "$*" >&2; }
die()  { _err "$*"; exit 1; }

# ─── helpers ──────────────────────────────────────────────────────────────────

# join_path <base> <rel>  →  base/rel  (handles trailing/leading slashes)
join_path() { echo "${1%/}/${2#/}"; }

require_docker() {
  command -v docker >/dev/null 2>&1 || die "docker not found in PATH"
  docker info >/dev/null 2>&1       || die "docker daemon is not reachable"
}

# Build the helper image on first use; subsequent calls are instant.
ensure_tool_image() {
  docker image inspect "$DOCKER_IMAGE" >/dev/null 2>&1 && return 0

  local tmpdir
  tmpdir="$(mktemp -d)"
  # shellcheck disable=SC2064
  trap "rm -rf '$tmpdir'" EXIT
  printf '%s\n' "$TOOL_DOCKERFILE" > "$tmpdir/Dockerfile"
  docker build --quiet -t "$DOCKER_IMAGE" "$tmpdir" >/dev/null
  rm -rf "$tmpdir"
}

volume_exists() { docker volume inspect "$1" >/dev/null 2>&1; }

ensure_volume() {
  local vol="$1"
  volume_exists "$vol" && return
  docker volume create "$vol" >/dev/null
}

ensure_confirmation() {
  local message="${1:-Continue?}"
  local reply
  printf "%s [y/N]: " "$message" >&2
  read -r reply
  case "$reply" in
    y|Y|yes|YES) return 0 ;;
    *) exit 0 ;;
  esac
}


# ─── usage ────────────────────────────────────────────────────────────────────

usage() {
  cat <<EOF
Usage:
  $SCRIPT_NAME list
  $SCRIPT_NAME search   <substring>
  $SCRIPT_NAME create   <volume>
  $SCRIPT_NAME remove   <volume> [-f|--force]
  $SCRIPT_NAME rename   <volume> <new-name>
  $SCRIPT_NAME clone    <volume> <new-volume>
  $SCRIPT_NAME snapshot <volume>
  $SCRIPT_NAME backup   [-f|--format zip|gz|xz|tar] [-v|--verbose] <volume> <archive>
  $SCRIPT_NAME restore  [-f|--force] <archive> <volume>
  $SCRIPT_NAME ls       <volume> [ls-options…] [/path]
  $SCRIPT_NAME find     <volume> [/path] [find-options…]
  $SCRIPT_NAME du       <volume> [/path]
  $SCRIPT_NAME cat      <volume> /absolute/path
  $SCRIPT_NAME tail     <volume> [-f] [-n N] /absolute/path
  $SCRIPT_NAME rm       <volume> [rm-flags…] /path […/path]
  $SCRIPT_NAME cp       [cp-flags…] <src> <dst>
  $SCRIPT_NAME mv       [mv-flags…] <src> <dst>
  $SCRIPT_NAME chown    <volume> [chown-flags…] <owner[:group]> /path
  $SCRIPT_NAME chmod    <volume> [chmod-flags…] <mode> /path
  $SCRIPT_NAME install-completion [--shell bash|zsh] [--print]

Endpoint syntax (cp / mv):
  vol:<volume>:/absolute/path   — a path inside a named volume
  dst:/absolute/or/relative     — a path on the host (relative to CWD)

Examples:
  $SCRIPT_NAME backup   my_volume my_volume.tar.xz
  $SCRIPT_NAME restore  my_volume.tar.xz my_volume_restored
  $SCRIPT_NAME snapshot my_volume
  $SCRIPT_NAME find     my_volume /var/log -name '*.log' -mtime -7
  $SCRIPT_NAME du       my_volume /var
  $SCRIPT_NAME chown    my_volume www-data:www-data /var/www
  $SCRIPT_NAME cp       vol:app_data:/config.json dst:./backup/config.json
EOF
}

# ─── volume listing / search ──────────────────────────────────────────────────

cmd_list() {
  docker volume ls --format '{{.Name}}'
}

cmd_search() {
  [[ $# -ge 1 ]] || die "'search' requires a substring argument"
  cmd_list | grep -F -- "$1" || true
}

# ─── create ───────────────────────────────────────────────────────────────────

cmd_create() {
  [[ $# -ge 1 ]] || die "'create' requires a volume name"
  local vol="$1"
  volume_exists "$vol" && die "volume '$vol' already exists"
  docker volume create "$vol"
}

# ─── remove ───────────────────────────────────────────────────────────────────

cmd_remove() {
  [[ $# -ge 1 ]] || die "'remove' requires <volume>"
  local vol="$1"
  shift
  local force=""
  while [[ $# -gt 0 ]]; do
    case "$1" in
      -f|--force) force=1; shift ;;
      *)          die "unknown option: $1" ;;
    esac
  done
  volume_exists "$vol" || die "volume '$vol' does not exist"
  [[ -n "$force" ]] || ensure_confirmation "Do you really want to remove volume '$vol'?"
  docker volume rm "$vol"
}

# ─── rename ───────────────────────────────────────────────────────────────────

cmd_rename() {
  [[ $# -ge 2 ]] || die "'rename' requires <volume> and <new-name>"
  local src_vol="$1" dst_vol="$2"
  [[ "$src_vol" != "$dst_vol" ]] || die "'rename': source and destination must differ"
  volume_exists "$src_vol" || die "volume '$src_vol' does not exist"

  if volume_exists "$dst_vol"; then
    ensure_confirmation "volume '$dst_vol' already exists, override?"
    docker volume rm "$dst_vol" >/dev/null
  fi

  docker volume create "$dst_vol" >/dev/null
  docker run --rm \
    -v "$src_vol:/src:ro" \
    -v "$dst_vol:/dst:rw" \
    "$DOCKER_IMAGE" \
    sh -c "cp -a /src/. /dst/"
  docker volume rm "$src_vol" >/dev/null
  echo "Renamed '$src_vol' → '$dst_vol'"
}

# ─── clone ────────────────────────────────────────────────────────────────────

cmd_clone() {
  [[ $# -ge 2 ]] || die "'clone' requires <volume> and <new-volume>"
  local src_vol="$1" dst_vol="$2"
  [[ "$src_vol" != "$dst_vol" ]] || die "'clone': source and destination must differ"
  volume_exists "$src_vol" || die "volume '$src_vol' does not exist"
  if volume_exists "$dst_vol"; then
    ensure_confirmation "volume '$dst_vol' already exists, override?"
    docker volume rm "$dst_vol" >/dev/null
  fi
  docker volume create "$dst_vol" >/dev/null
  docker run --rm \
    -v "$src_vol:/src:ro" \
    -v "$dst_vol:/dst:rw" \
    "$DOCKER_IMAGE" \
    sh -c "cp -a /src/. /dst/"
  echo "Cloned '$src_vol' → '$dst_vol'"
}

# ─── snapshot ─────────────────────────────────────────────────────────────────

cmd_snapshot() {
  [[ $# -ge 1 ]] || die "'snapshot' requires a volume name"
  local src_vol="$1"
  volume_exists "$src_vol" || die "volume '$src_vol' does not exist"

  local timestamp dst_vol
  timestamp="$(date +%Y%m%dT%H%M%S)"
  dst_vol="${src_vol}_${timestamp}"

  docker volume create "$dst_vol" >/dev/null
  docker run --rm \
    -v "$src_vol:/src:ro" \
    -v "$dst_vol:/dst:rw" \
    "$DOCKER_IMAGE" \
    sh -c "cp -a /src/. /dst/"
  echo "Snapshot '$dst_vol'"
}

# ─── backup / archive ─────────────────────────────────────────────────────────

cmd_backup() {
  local format="" verbose="" explicit_format=""

  while [[ "${1:-}" == -* ]]; do
    case "$1" in
      -f|--format)  format="${2:?'--format requires an argument'}"; explicit_format=1; shift 2 ;;
      -v|--verbose) verbose=1; shift ;;
      --)           shift; break ;;
      *)            die "unknown option: $1" ;;
    esac
  done

  [[ $# -ge 2 ]] || die "'backup' requires <volume> and <archive>"
  local vol="$1" output="$2"
  [[ -n "$vol"    ]] || die "'backup': volume name must not be empty"
  [[ -n "$output" ]] || die "'backup': archive filename must not be empty"
  volume_exists "$vol" || die "volume '$vol' does not exist"

  if [[ -z "$explicit_format" ]]; then
    case "$output" in
      *.zip)           format="zip" ;;
      *.tar.gz|*.tgz)  format="gz"  ;;
      *.tar.xz|*.txz)  format="xz"  ;;
      *.tar)            format="tar" ;;
      *) die "'backup': cannot infer format from '$output' — use --format (zip, gz, xz, tar)" ;;
    esac
  fi

  local compress_cmd
  case "$format" in
    zip) compress_cmd="zip -r${verbose:+v} /out/$output ." ;;
    gz)  compress_cmd="tar c${verbose:+v}zf /out/$output ." ;;
    xz)  compress_cmd="tar c${verbose:+v}Jf /out/$output ." ;;
    tar) compress_cmd="tar c${verbose:+v}f  /out/$output ." ;;
    *)   die "unsupported format '$format' (expected: zip, gz, xz, tar)" ;;
  esac

  docker run --rm \
    -v "$vol:/data:ro" \
    -v "$(pwd):/out" \
    "$DOCKER_IMAGE" \
    sh -c "cd /data && $compress_cmd"
}

# ─── restore ─────────────────────────────────────────────────────────────────

cmd_restore() {
  local force=""

  while [[ "${1:-}" == -* ]]; do
    case "$1" in
      -f|--force) force=1; shift ;;
      --)         shift; break ;;
      *)          die "unknown option: $1" ;;
    esac
  done

  [[ $# -ge 2 ]] || die "'restore' requires <archive> and <volume>"
  local archive="$1" vol="$2"
  [[ -n "$archive" ]] || die "'restore': archive path must not be empty"
  [[ -n "$vol"     ]] || die "'restore': volume name must not be empty"
  [[ -f "$archive" ]] || die "archive '$archive' does not exist or is not a file"

  if volume_exists "$vol"; then
    [[ -n "$force" ]] || ensure_confirmation "volume '$vol' already exists, override?"
    # BUG FIX #1: was "$dst_vol" (undefined variable), must be "$vol"
    docker volume rm "$vol" >/dev/null
  fi
  docker volume create "$vol" >/dev/null

  local archive_abs archive_base
  archive_abs="$(cd "$(dirname "$archive")" && pwd)/$(basename "$archive")"
  archive_base="$(basename "$archive")"

  local extract_cmd
  case "$archive_base" in
    *.zip)           extract_cmd="unzip -o /in/$archive_base -d /data" ;;
    *.tar.gz|*.tgz)  extract_cmd="tar xzf /in/$archive_base -C /data" ;;
    *.tar.xz|*.txz)  extract_cmd="tar xJf /in/$archive_base -C /data" ;;
    *.tar)            extract_cmd="tar xf  /in/$archive_base -C /data" ;;
    *) die "cannot detect format of '$archive' (expected .zip, .tar.gz, .tgz, .tar.xz, .txz, .tar)" ;;
  esac

  docker run --rm \
    -v "$vol:/data:rw" \
    -v "$(dirname "$archive_abs"):/in:ro" \
    "$DOCKER_IMAGE" \
    sh -c "$extract_cmd"

  echo "Restored '$archive' → '$vol'"
}

# ─── ls ───────────────────────────────────────────────────────────────────────

cmd_ls() {
  [[ $# -ge 1 ]] || die "'ls' requires a volume name"
  local vol="$1"; shift
  volume_exists "$vol" || die "volume '$vol' does not exist"

  local ls_opts=() path="/"
  for arg in "$@"; do
    if [[ "$arg" == /* ]]; then path="$arg"
    else ls_opts+=("$arg")
    fi
  done

  docker run --rm \
    -v "$vol:/data:ro" \
    "$DOCKER_IMAGE" \
    ls "${ls_opts[@]+"${ls_opts[@]}"}" "$(join_path /data "$path")"
}

# ─── find ─────────────────────────────────────────────────────────────────────

cmd_find() {
  [[ $# -ge 1 ]] || die "'find' requires a volume name"
  local vol="$1"; shift
  volume_exists "$vol" || die "volume '$vol' does not exist"

  local path="/"
  if [[ $# -ge 1 && "${1:-}" == /* ]]; then
    path="$1"; shift
  fi

  docker run --rm \
    -v "$vol:/data:ro" \
    "$DOCKER_IMAGE" \
    find "$(join_path /data "$path")" "$@"
}

# ─── du ───────────────────────────────────────────────────────────────────────

cmd_du() {
  [[ $# -ge 1 ]] || die "'du' requires a volume name"
  local vol="$1"; shift
  volume_exists "$vol" || die "volume '$vol' does not exist"

  local path="/"
  if [[ $# -ge 1 && "${1:-}" == /* ]]; then
    path="$1"; shift
  fi

  docker run --rm \
    -v "$vol:/data:ro" \
    "$DOCKER_IMAGE" \
    du -sh "$(join_path /data "$path")" "$@"
}

# ─── cat / tail ───────────────────────────────────────────────────────────────

cmd_cat_tail() {
  local subcmd="$1"; shift
  [[ $# -ge 1 ]] || die "'$subcmd' requires a volume name"
  local vol="$1"; shift
  volume_exists "$vol" || die "volume '$vol' does not exist"

  local cmd_opts=() path=""

  while [[ $# -gt 0 ]]; do
    case "$1" in
      -f)   cmd_opts+=("-f"); shift ;;
      -n)   cmd_opts+=("-n" "${2:?'-n requires a value'}"); shift 2 ;;
      -n*)  cmd_opts+=("$1"); shift ;;
      -*)   cmd_opts+=("$1"); shift ;;
      /*)   path="$1"; shift; break ;;
      *)    die "'$subcmd': expected an absolute path, got '$1'" ;;
    esac
  done

  [[ -n "$path" ]] || die "'$subcmd' requires an absolute file path"

  local full_path
  full_path="$(join_path /data "$path")"

  local tty_flag=""
  [[ "$subcmd" == "tail" ]] && [[ " ${cmd_opts[*]:-} " == *" -f "* ]] && tty_flag="-it"

  # shellcheck disable=SC2086
  docker run --rm $tty_flag \
    -v "$vol:/data:ro" \
    "$DOCKER_IMAGE" \
    sh -c '[ -f "$1" ] || { echo "error: not a regular file: $2" >&2; exit 1; }
           shift 2; exec "$@"' \
    -- "$full_path" "$path" "$subcmd" "${cmd_opts[@]+"${cmd_opts[@]}"}" "$full_path"
}

# ─── rm ───────────────────────────────────────────────────────────────────────

cmd_rm() {
  [[ $# -ge 1 ]] || die "'rm' requires a volume name"
  local vol="$1"; shift
  volume_exists "$vol" || die "volume '$vol' does not exist"

  local rm_opts=() paths=()

  while [[ $# -gt 0 ]]; do
    case "$1" in
      # BUG FIX #2: missing semicolons between commands on these three lines
      --recursive|--force|--verbose) rm_opts+=("$1"); shift ;;
      -[rfvR]*)                      rm_opts+=("$1"); shift ;;
      # BUG FIX #3: `shift break` is not valid — must be `shift; break`
      --)                            shift; break ;;
      /*)                            paths+=("$1"); shift ;;
      -*)                            die "unknown rm option: $1" ;;
      *)                             die "'rm': expected an absolute path, got '$1'" ;;
    esac
  done
  while [[ $# -gt 0 ]]; do paths+=("$1"); shift; done

  [[ ${#paths[@]} -ge 1 ]] || die "'rm' requires at least one absolute path"

  local p
  for p in "${paths[@]}"; do
    [[ "$p" == /* ]] || die "'rm': path must be absolute (got '$p')"
    [[ "$p" != "/" ]] || die "'rm': refusing to remove volume root '/'"
  done

  local container_paths=()
  for p in "${paths[@]}"; do
    container_paths+=("$(join_path /data "$p")")
  done

  docker run --rm \
    -v "$vol:/data:rw" \
    "$DOCKER_IMAGE" \
    rm "${rm_opts[@]+"${rm_opts[@]}"}" "${container_paths[@]}"
}

# ─── endpoint parser ──────────────────────────────────────────────────────────
# Populates: EP_TYPE (vol|dst), EP_VOL, EP_PATH

parse_endpoint() {
  local spec="$1" require_exists="${2:-1}"

  [[ -n "$spec" ]] || die "empty endpoint specification"

  if [[ "$spec" == vol:*:* ]]; then
    local rest="${spec#vol:}"
    EP_TYPE="vol"
    EP_VOL="${rest%%:*}"
    EP_PATH="${rest#*:}"

    [[ -n "$EP_VOL"  ]] || die "endpoint '$spec': volume name is empty"
    [[ -n "$EP_PATH" ]] || die "endpoint '$spec': path is empty"
    [[ "$EP_PATH" == /* ]] || die "endpoint '$spec': path must be absolute (got '$EP_PATH')"

    if [[ "$require_exists" == "1" ]]; then
      volume_exists "$EP_VOL" || die "endpoint '$spec': volume '$EP_VOL' does not exist"
    fi

  elif [[ "$spec" == dst:* ]]; then
    EP_TYPE="dst"
    EP_VOL=""
    EP_PATH="${spec#dst:}"
    [[ -n "$EP_PATH" ]] || die "endpoint '$spec': path is empty"

  else
    die "invalid endpoint '$spec' — expected 'vol:<volume>:/path' or 'dst:/path'"
  fi
}

# ─── cp / mv engine ───────────────────────────────────────────────────────────

cmd_cp_mv() {
  local mode="$1"; shift
  [[ $# -ge 2 ]] || die "'$mode' requires source and destination endpoints"

  local src_spec="${*: -2:1}"
  local dst_spec="${*: -1:1}"
  local extra=("${@:1:$(( $# - 2 ))}")
  local flags="${extra[*]:-}"

  local EP_TYPE EP_VOL EP_PATH

  parse_endpoint "$src_spec" 1
  local src_type="$EP_TYPE" src_vol="$EP_VOL" src_path="$EP_PATH"

  parse_endpoint "$dst_spec" 0
  local dst_type="$EP_TYPE" dst_vol="$EP_VOL" dst_path="$EP_PATH"

  [[ "$dst_type" == "vol" ]] && ensure_volume "$dst_vol"

  case "${src_type}:${dst_type}" in

    dst:dst)
      [[ -e "$src_path" ]] || die "source '$src_path' does not exist"
      mkdir -p "$(dirname "$dst_path")"
      if [[ "$mode" == "cp" ]]; then
        cp ${flags:+$flags} "$src_path" "$dst_path"
      else
        mv ${flags:+$flags} "$src_path" "$dst_path"
      fi
      ;;

    vol:vol)
      local src_full dst_full
      src_full="$(join_path /src "$src_path")"
      dst_full="$(join_path /dst "$dst_path")"

      docker run --rm \
        -v "$src_vol:/src:ro" \
        -v "$dst_vol:/dst:rw" \
        "$DOCKER_IMAGE" \
        sh -c '[ -e "$1" ] || { echo "error: source does not exist: $2" >&2; exit 1; }
               mkdir -p "$(dirname "$3")"
               cp -a ${4:+$4} "$1" "$3"' \
        -- "$src_full" "$src_path" "$dst_full" "${flags:-}"

      if [[ "$mode" == "mv" ]]; then
        docker run --rm \
          -v "$src_vol:/src:rw" \
          "$DOCKER_IMAGE" \
          rm -rf "$src_full"
      fi
      ;;

    vol:dst)
      local src_full
      src_full="$(join_path /src "$src_path")"
      mkdir -p "$(dirname "$dst_path")"

      docker run --rm \
        -v "$src_vol:/src:ro" \
        -v "$(pwd):/out" \
        "$DOCKER_IMAGE" \
        sh -c '[ -e "$1" ] || { echo "error: source does not exist: $2" >&2; exit 1; }
               cp -a ${3:+$3} "$1" "/out/$4"' \
        -- "$src_full" "$src_path" "${flags:-}" "$dst_path"

      if [[ "$mode" == "mv" ]]; then
        docker run --rm \
          -v "$src_vol:/src:rw" \
          "$DOCKER_IMAGE" \
          rm -rf "$src_full"
      fi
      ;;

    dst:vol)
      [[ -e "$src_path" ]] || die "source '$src_path' does not exist"
      local dst_full
      dst_full="$(join_path /dst "$dst_path")"

      docker run --rm \
        -v "$dst_vol:/dst:rw" \
        -v "$(pwd):/in" \
        "$DOCKER_IMAGE" \
        sh -c 'mkdir -p "$(dirname "$1")"
               cp -a ${2:+$2} "/in/$3" "$1"' \
        -- "$dst_full" "${flags:-}" "$src_path"

      [[ "$mode" == "mv" ]] && rm -rf "$src_path"
      ;;

    *)
      die "unsupported endpoint combination: ${src_type} → ${dst_type}"
      ;;
  esac
}

# ─── chown / chmod ────────────────────────────────────────────────────────────

cmd_chown_chmod() {
  local subcmd="$1"; shift
  [[ $# -ge 1 ]] || die "'$subcmd' requires a volume name"
  local vol="$1"; shift
  volume_exists "$vol" || die "volume '$vol' does not exist"

  [[ $# -ge 2 ]] || die "'$subcmd' requires <value> and /path"

  local path="${*: -1:1}"
  local value="${*: -2:1}"
  local extra=("${@:1:$(( $# - 2 ))}")
  local flags="${extra[*]:-}"

  [[ "$path" == /* ]] || die "'$subcmd': path must be absolute (got '$path')"
  [[ -n "$value" ]]   || die "'$subcmd': value must not be empty"

  local full_path
  full_path="$(join_path /data "$path")"

  docker run --rm \
    -v "$vol:/data:rw" \
    "$DOCKER_IMAGE" \
    sh -c '[ -e "$1" ] || { echo "error: path does not exist: $2" >&2; exit 1; }
           shift 2; exec "$@"' \
    -- "$full_path" "$path" "$subcmd" ${flags:+$flags} "$value" "$full_path"
}

# ─── shell completion ─────────────────────────────────────────────────────────

_dvt_completion_bash() {
  local bin="$1"
  cat <<'BASH'
_dvt_volumes() { docker volume ls --format '{{.Name}}' 2>/dev/null; }

_docker_volume_toolbox() {
  local cur prev words cword
  if declare -f _init_completion >/dev/null 2>&1; then
    _init_completion || return
  else
    words=("${COMP_WORDS[@]}")
    cword=$COMP_CWORD
    cur="${words[$cword]}"
    prev="${words[$cword-1]}"
  fi

  local commands='list search create remove rename clone snapshot backup restore ls find du cat tail rm cp mv chown chmod install-completion help'

  local cmd="" cmd_idx=0 i
  for (( i = 1; i < ${#words[@]}; i++ )); do
    if [[ "${words[i]}" != -* ]]; then
      cmd="${words[i]}"; cmd_idx=$i; break
    fi
  done

  if [[ -z "$cmd" || $cword -le $cmd_idx ]]; then
    COMPREPLY=( $(compgen -W "$commands" -- "$cur") ); return
  fi

  local arg_pos=$(( cword - cmd_idx ))

  case "$cmd" in
    list|search|create)
      ;;
    ls|cat|tail|find|du|rm|snapshot|chown|chmod)
      [[ $arg_pos -eq 1 ]] && COMPREPLY=( $(compgen -W "$(_dvt_volumes)" -- "$cur") )
      ;;
    clone|rename|remove)
      [[ $arg_pos -eq 1 ]] && COMPREPLY=( $(compgen -W "$(_dvt_volumes)" -- "$cur") )
      ;;
    backup)
      if [[ "$prev" == "--format" || "$prev" == "-f" ]]; then
        COMPREPLY=( $(compgen -W "gz xz zip tar" -- "$cur") )
      elif [[ $arg_pos -eq 1 ]]; then
        COMPREPLY=( $(compgen -W "$(_dvt_volumes)" -- "$cur") )
      fi
      ;;
    restore)
      if [[ $arg_pos -eq 1 ]]; then
        COMPREPLY=( $(compgen -f -- "$cur") ); compopt -o filenames
      elif [[ $arg_pos -eq 2 ]]; then
        COMPREPLY=( $(compgen -W "$(_dvt_volumes)" -- "$cur") )
      fi
      ;;
    cp|mv)
      if [[ "$cur" == vol:* ]]; then
        COMPREPLY=( $(compgen -W "$(
          docker volume ls --format '{{.Name}}' 2>/dev/null \
            | awk '{print "vol:" $0 ":/"}')" -- "$cur") )
      elif [[ "$cur" == dst:* ]]; then
        local path_part="${cur#dst:}"
        COMPREPLY=( $(compgen -f -- "$path_part" | awk '{print "dst:" $0}') )
        compopt -o filenames
      else
        COMPREPLY=( $(compgen -W "vol: dst:" -- "$cur") )
      fi
      ;;
    search)
      COMPREPLY=( $(compgen -W "$(_dvt_volumes)" -- "$cur") )
      ;;
  esac
}
BASH
  printf 'complete -F _docker_volume_toolbox %s\n' "$bin"
}

_dvt_completion_zsh() {
  local bin="$1"
  cat <<'ZSH'
_dvt_volumes() { docker volume ls --format '{{.Name}}' 2>/dev/null }

_docker_volume_toolbox() {
  # Guard: if zsh is completing a plain path/file (e.g. ./docker-volume.sh d<TAB>)
  # the service will be the script path; redirect to our handler.
  [[ "$service" == *docker-volume* || "$service" == *dvt* ]] || return 1

  local state line
  typeset -A opt_args

  local -a commands
  commands=(
    'list:List all volumes'
    'search:Search volumes by substring'
    'create:Create a new volume'
    # BUG FIX #4: typo 'Remoce' → 'Remove'
    'remove:Remove a volume'
    'rename:Rename a volume'
    'clone:Clone a volume'
    'snapshot:Snapshot a volume with a timestamp'
    'backup:Archive a volume to a file'
    'restore:Restore a volume from an archive'
    'ls:List files inside a volume'
    'find:Find files inside a volume'
    'du:Disk usage of a volume or path'
    'cat:Print a file from a volume'
    'tail:Tail a file from a volume'
    'rm:Remove paths inside a volume'
    'cp:Copy files between volumes and host'
    'mv:Move files between volumes and host'
    'chown:Change ownership of a path in a volume'
    'chmod:Change permissions of a path in a volume'
    'install-completion:Install shell completion'
    'help:Show usage'
  )

  _arguments -C \
    '1: :->cmd' \
    '*:: :->args'

  case $state in
    cmd)
      _describe 'command' commands
      ;;
    args)
      local -a volumes
      volumes=("${(@f)$(_dvt_volumes)}")

      case $words[1] in
        ls|cat|tail|find|du|rm|snapshot|chown|chmod)
          (( CURRENT == 2 )) && _wanted volumes expl 'volume' compadd -a volumes
          ;;
        clone|rename|remove)
          case $CURRENT in
            2) _wanted volumes expl 'source volume' compadd -a volumes ;;
          esac
          ;;
        backup)
          case $words[CURRENT-1] in
            --format|-f) compadd gz xz zip tar ;;
            *)
              case $CURRENT in
                2) _wanted volumes expl 'volume' compadd -a volumes ;;
                *) _arguments '--format[archive format]:format:(gz xz zip tar)' \
                               '--verbose[verbose output]' ;;
              esac
              ;;
          esac
          ;;
        restore)
          case $CURRENT in
            2) _files ;;
            3) _wanted volumes expl 'volume' compadd -a volumes ;;
          esac
          ;;
        cp|mv)
          local cur="${words[$CURRENT]}"
          if [[ "$cur" == vol:* ]]; then
            local -a vol_completions
            vol_completions=("${(@f)$(
              docker volume ls --format '{{.Name}}' 2>/dev/null \
                | awk '{print "vol:" $0 ":/"}'
            )}")
            compadd -S '' -a vol_completions
          elif [[ "$cur" == dst:* ]]; then
            local path="${cur#dst:}"
            local -a matches
            matches=("${(@f)$(compgen -f -- "$path" 2>/dev/null)}")
            compadd -S '' "${matches[@]/#/dst:}"
          else
            compadd -S '' 'vol:' 'dst:'
          fi
          ;;
        search)
          _wanted volumes expl 'volume' compadd -a volumes
          ;;
      esac
      ;;
  esac
}
ZSH
  # Register completion for:
  #   - bare command name (e.g. docker-volume.sh)
  #   - path-prefixed invocation (e.g. ./docker-volume.sh)
  printf 'compdef _docker_volume_toolbox %s\n' "$bin"
  printf 'compdef _docker_volume_toolbox ./%s\n' "$bin"
}

cmd_install_completion() {
  local bin print="" shell=""
  bin="$(basename "$0")"

  while [[ "${1:-}" == -* ]]; do
    case "$1" in
      --print)  print=1;    shift ;;
      --shell)  shell="$2"; shift 2 ;;
      *)        die "unknown option: $1" ;;
    esac
  done

  if [[ -z "$shell" ]]; then
    if [[ -n "${ZSH_VERSION:-}" ]]; then
      shell="zsh"
    else
      shell="bash"
    fi
  fi

  local emit_fn
  case "$shell" in
    bash) emit_fn=_dvt_completion_bash ;;
    zsh)  emit_fn=_dvt_completion_zsh  ;;
    *)    die "unsupported shell '$shell' (expected: bash, zsh)" ;;
  esac

  if [[ -n "$print" ]]; then
    "$emit_fn" "$bin"
    return
  fi

  local dest
  case "$shell" in
    bash)
      if [[ -d "${BASH_COMPLETION_USER_DIR:-$HOME/.local/share/bash-completion}/completions" ]]; then
        dest="${BASH_COMPLETION_USER_DIR:-$HOME/.local/share/bash-completion}/completions/$bin"
      elif [[ -d /usr/local/share/bash-completion/completions ]]; then
        dest="/usr/local/share/bash-completion/completions/$bin"
      elif [[ -d /etc/bash_completion.d ]]; then
        dest="/etc/bash_completion.d/$bin"
      else
        die "no bash-completion directory found; use --print to source manually:
  source <($SCRIPT_NAME install-completion --print)"
      fi
      ;;
    zsh)
      # Strip .sh suffix — zsh autoload breaks on filenames containing dots
      local bin_stem="${bin%.sh}"
      local fpath_dir
      for fpath_dir in "${fpath[@]:-}"; do
        [[ -z "$fpath_dir" ]] && continue
        if [[ -d "$fpath_dir" && -w "$fpath_dir" ]]; then
          dest="$fpath_dir/_$bin_stem"
          break
        fi
      done
      if [[ -z "${dest:-}" ]]; then
        dest="${ZDOTDIR:-$HOME}/.zsh/completions/_$bin_stem"
        mkdir -p "$(dirname "$dest")"
        echo "Note: add 'fpath=($(dirname "$dest") \$fpath)' to your .zshrc if not already present"
      fi
      ;;
  esac

  "$emit_fn" "$bin" > "$dest"
  echo "Completion installed → $dest"
  case "$shell" in
    bash) echo "Reload with: source '$dest'  (or open a new shell)" ;;
    zsh)
	  echo "Reload with: compinit  (or open a new shell)"
	  echo "or source manually: source <($SCRIPT_NAME install-completion --print)"
	  ;;
  esac
}

# ─── main ─────────────────────────────────────────────────────────────────────

main() {
  case "${1:-}" in
    install-completion) shift; cmd_install_completion "$@"; exit 0 ;;
    -h|--help|help)     usage;                              exit 0 ;;
  esac

  require_docker
  ensure_tool_image

  [[ $# -ge 1 ]] || { usage; exit 1; }

  local cmd="$1"; shift

  case "$cmd" in
    list)     cmd_list ;;
    search)   cmd_search "$@" ;;
    create)   cmd_create "$@" ;;
    remove)   cmd_remove "$@" ;;
    rename)   cmd_rename "$@" ;;
    clone)    cmd_clone "$@" ;;
    snapshot) cmd_snapshot "$@" ;;
    backup)   cmd_backup "$@" ;;
    restore)  cmd_restore "$@" ;;
    ls)       cmd_ls "$@" ;;
    find)     cmd_find "$@" ;;
    du)       cmd_du "$@" ;;
    cat)      cmd_cat_tail cat "$@" ;;
    tail)     cmd_cat_tail tail "$@" ;;
    rm)       cmd_rm "$@" ;;
    cp)       cmd_cp_mv cp "$@" ;;
    mv)       cmd_cp_mv mv "$@" ;;
    chown)    cmd_chown_chmod chown "$@" ;;
    chmod)    cmd_chown_chmod chmod "$@" ;;
    *)        usage; exit 1 ;;
  esac
}

main "$@"
