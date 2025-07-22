#!/bin/bash
lookandfeeltool -a org.kde.breezedark.desktop
rm broken-heart.svg
ln -s broken-heart-dark-theme.svg broken-heart.svg
echo "dark" > current-theme
