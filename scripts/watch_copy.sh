#!/bin/bash

echo "Watching copy directory"
while true; do
  inotifywait -rqe MODIFY copy --format "%w%f" | \
    xargs pandoc -f markdown -t html -S -- | grep -v "^<!--"
done
