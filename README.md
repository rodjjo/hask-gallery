# hask-gallery

## Attention

Status: **The master branch has an usable version**

**This project has a simple gallery for songs, pictures and videos (.mp4). It has few features but it will have more features soon.**

Optionaly you can download put ffprobe binary together with hask-gallery-exe.
If you do it will use it to retrive informations about the video files.
You can Also put ffproble location at your PATH environment variable.

You can download ffmpeg and ffprobe here [ffmpeg builds](https://www.ffmpeg.org/download.html)

## About this project

This project was created just to learn haskell. It will be in early future a gallery to listen to music, to watch video clips or look at pictures.

## How this project works

* First it is necessary to configure the path to the galleries (videos, musics and pictures) (instructions bellow)
* Second you call hask-gallery-exe with a parameter `refresh`
* The program will scan the directory you passed at the first step looking for media files and storing their information in a file.
* Finally you can run the hask-gallery without any parameter and open the browser at localhost:8080 url.

## Usage

Configure the gallery
```bash
# set the title of our gallery web page title
hask-gallery-exe gal-title "Title of my gallery"
# set the path to look for video files (such music video clips, movies and etc)
hask-gallery-exe gal-videos-path "/path/to/video-clips"
hask-gallery-exe gal-musics-path "/path/to/musics"
hask-gallery-exe gal-pictures-path "/path/to/pictures"
```

## Tip
Leave a file named cover.jpg inside the music directories.


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
