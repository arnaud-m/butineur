#!/bin/sh

TARGET=../www/butineur.css
cp bootstrap.min.css $TARGET
yuicompressor --type css butineur-extra.css >> $TARGET
yuicompressor --type css logo_ove.css >> $TARGET
