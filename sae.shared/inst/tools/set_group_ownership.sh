#!/bin/bash
# Sets the group ownership to $group for $dir recursively

group="$1"
dir="$2"

echo "Setting group ownership to $group recursively for: $dir"
echo "..."
# -h flag changes the group ownership of symlink, doesn't touch actual files.
chgrp -hR "$group" $dir
echo "Done!"