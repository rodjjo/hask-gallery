# hask-gallery

## Attention

**This project is in early development state and it does not have working releases yet.**

Currently this project depends on ffprobe being present and configured at your PATH environment variable.

You can download ffmpeg and ffprobe here [ffmpeg builds](https://ffmpeg.zeranoe.com/builds/)

## About this project

This project was created just to learn haskell. It will be in early future a gallery to listen to music, to watch video clips or look at pictures.
HaskGallery intend to be used for a single user at his computer and it's necessary to open an url in a web browser to use it.

## Final user usage

Configure the gallery
```bash
# set the title of our gallery web page title
hask-gallery-exe gal-title "Title of my gallery"
# set the path to look for video files (such music video clips, movies and etc)
hask-gallery-exe gal-video-path "/path/to/video-clips"
#not implemented yet: hask-gallery-exe gal-music-path "/path/to/musics"
#not implemented yet: hask-gallery-exe gal-picture-path "/path/to/pictures"
```

Update the gallery
```bash
hask-gallery-exe refresh
```

Run the gallery program
```bash
hask-gallery-exe --port 8080
```

Then open the webbrowser to use it
```text
url to open at your browser: http://localhost:8080
```


## Development commands

```bash
# build the project
stack build
# build and run the tests
stack test
# run the gallery
stack run hask-gallery-exe
# show avaliable management commands
stack run hask-gallery-exe help
```
