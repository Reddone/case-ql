#!/bin/bash

OPT_IND=1

function show_help() {
  echo "External services:"
  echo "  -h show this help"
  echo "  -u start external services"
  echo "  -d stop external services"
}

while getopts "h?u?d?" opt; do
  case "$opt" in
  h)
    show_help
    exit 0
    ;;
  u)
    docker-compose up -d db adminer
    exit 0
    ;;
  d)
    docker-compose down
    exit 0
    ;;
  *)
    ;;
  esac
done

shift $((OPT_IND - 1))

[ "${1:-}" = "--" ] && shift
show_help
