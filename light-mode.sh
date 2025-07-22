#!/bin/bash
lookandfeeltool -a org.kde.breeze.desktop
rm broken-heart.svg
ln -s broken-heart-light-theme.svg broken-heart.svg
echo "light" > current-theme
