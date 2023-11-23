#!/usr/bin/env bash
set -euxo pipefail

# Input
: "${HOST:=localhost:3000}"

# 初期化
psql postgres://postgres@localhost:5432/postgres <<< 'truncate table blog_posts, people;'

# User投入
curl -sSf -X POST "http://$HOST/users/taro?age=15"
# User取得
user=$(curl -sSf "http://$HOST/users/taro")
diff -u <(jq <<<"$user" '.name') <(jq -n '"taro"')
diff -u <(jq <<<"$user" '.age')  <(jq -n '15')

# Blog投入
curl -sSf -X POST "http://$HOST/users/taro/blog_posts?title=Hello"
curl -sSf -X POST "http://$HOST/users/taro/blog_posts?title=World"
# Blog取得
blogs=$(curl -sSf "http://$HOST/users/taro/blog_posts")
diff -u <(jq <<<"$blogs" '.[0]') <(jq -n '"Hello"')
diff -u <(jq <<<"$blogs" '.[1]') <(jq -n '"World"')
