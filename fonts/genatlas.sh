#! /bin/sh

# Generate one atlas for each font (called variants).

msdf-atlas-gen $(
  # Printable ASCII characters
  echo '-charset charset.txt'
  # Bitmap output
  echo '-type hardmask'
  echo '-format png'
  echo '-json atlas.json'
  echo '-imageout glyphs.png'
  # Variant 1 Chicago
  echo '-font pixChicago.ttf'
  echo '-and'
  # Variant 2 Minecraft
  echo '-font Minecraft.ttf'
)
