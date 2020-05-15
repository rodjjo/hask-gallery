function GalleryModule() {
    let currentVideoIdx = 0;
    let currentPictureIdx = 0;
    let currentMusicIdx = 0;
    let videos = [];
    let pictures = [];
    let musics = [];
    let currentPage = "";

    function nextVideo() {
        currentVideoIdx += 1;
        displayVideoAtCurrentIndex();
    }

    function previousVideo() {
        if (currentVideoIdx > 0) {
            currentVideoIdx -= 1;
            displayVideoAtCurrentIndex();
        }
    }

    function nextPicture() {
        currentPictureIdx += 1;
        displayPictureAtCurrentIndex();
    }

    function previousPicture() {
        if (currentPictureIdx > 0) {
            currentPictureIdx -= 1;
            displayPictureAtCurrentIndex();
        }
    }

    function nextMusic() {
        currentMusicIdx += 1;
        displayMusicAtCurrentIndex();
    }

    function previousMusic() {
        if (currentMusicIdx > 0) {
            currentMusicIdx -= 1;
            displayMusicAtCurrentIndex();
        }
    }

    const pageControls = {
        "videos": {
            "controls": ["id-video-page", "id-bottom-panel-video"],
            "order": 0,
            "toggleControl": "id-toggle-video",
            "streamingControl": "id-video",
            "topBarControl": "id-top-panel",
            "bottomBarControl": "id-bottom-panel-video",
            "nextFunction": nextVideo,
            "previousFunction": previousVideo,
        },
        "music": {
            "controls": ["id-audio-page", "id-bottom-panel-music"],
            "order": 1,
            "toggleControl": "id-toggle-music",
            "topBarControl": "id-top-panel",
            "bottomBarControl": "id-bottom-panel-music",
            "streamingControl": "id-audio",
            "nextFunction": nextMusic,
            "previousFunction": previousMusic,
        },
        "pictures": {
            "controls": ["id-picture-page", "id-bottom-panel-picture"],
            "order": 1,
            "topBarControl": "id-top-panel",
            "toggleControl": "id-toggle-picture",
            "nextFunction": nextPicture,
            "streamingControl": "id-audio",
            "previousFunction": previousPicture,
            "bottomBarControl": "id-bottom-panel-picture",
        }
    };

    const streamIterationControls =[
        $("#id-previous"),
        $("#id-next"),
        $("#id-pause-play"),
        $("#id-maximize")
    ]

    let streamIterationVisibleTimeout = 2;

    setInterval(() => {
        if (streamIterationVisibleTimeout > 0) {
            streamIterationVisibleTimeout -= 1;
        } else {
            streamIterationControls.forEach((c) => { c.removeClass("buttons-visible");});
        }
    }, 200);

    const doc = document;
    const de = doc.documentElement;
    const fullScreenTogleFunctions = [
        ( doc.cancelFullScreen || doc.mozCancelFullScreen || doc.webkitCancelFullScreen || (() => {})),
        ( de.requestFullscreen || de.mozRequestFullScreen || (
                () => {
                    if (de.webkitRequestFullscreen) de.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT)
                }))
    ];

    const IsFullScreen = () => (!(!doc.fullscreenElement && !doc.mozFullScreenElement && !doc.webkitFullscreenElement));

    const format = function() {
        const args = arguments;
        return this.replace(/{(\d+)}/g, function(match, number) {
            return typeof args[number] != 'undefined'
                ? args[number]
                : match
            ;
        });
    };

    function get(template) {
        return function() {
            const args = Array.from(arguments).slice(0, -1);
            const callback = Array.from(arguments).slice(-1)[0];
            const url = format.apply(template, args);
            $.get(url, callback);
        }
    }

    function composeURI(base) {
        return function(path) {
            return `${base}${encodeURI(path).replace(/#/g, '%23')}`
        }
    }

    const videoFilesURL = composeURI('/files/videos');
    const musicFilesURL = composeURI('/files/musics');
    const pictureFilesURL = composeURI('/files/pictures');

    const getVideos = get('/videos/{0}/{1}/');
    const getMusics = get('/musics/{0}/{1}/');
    const getPictures = get('/pictures/{0}/{1}/');
    const getCover = get('/covers{0}');

    function dePaginator(getFunction) {
        return function dePaginate(currentSeed, page, currentList, resultCb) {
            getFunction(currentSeed, page, function(data) {
                if (data.pagination.page + 1 < data.pagination.pageCount)
                    dePaginate(data.pagination.randomSeed, data.pagination.page + 1, currentList.concat(data.items), resultCb);
                else
                    resultCb(currentList.concat(data.items))
            });
        };
    }

    function updateVideoInfo(data) {
        $('#content-info').text(data.videoPath);
        $('#id-video-page-info').text(`${currentVideoIdx + 1} of ${videos.length} videos`);
        $('#id-video-seekbar')[0].time = 0;
    }

    function updateMusicInfo(data) {
        $('#content-info-music').text(data.musicPath);
        $('#id-audio-page-info').text(`${currentMusicIdx + 1} of ${musics.length} songs`);
        $('#id-audio-seekbar')[0].time = 0;
    }
    function updatePictureInfo(data) {
        $("#id-path-picture").text(`${data.picturePath}`);
        $("#id-count-picture").text(`${currentPictureIdx + 1} of ${pictures.length} pictures`);
    }

    function changeVideo(data) {
        const videoComponent = $('#id-video');
        videoComponent.attr('src', videoFilesURL(data.videoPath));
        videoComponent[0].load();
        if (currentPage === 'videos') {
            videoComponent[0].play();
        }
    }

    function changeMusic(data) {
        const musicComponent = $('#id-audio');
        musicComponent.attr('src', musicFilesURL(data.musicPath));
        getCover(data.musicPath, (data)=> {
            if (data.coverPath) {
                $('#id-audio-cover').css('display', 'block');
                $('#id-audio-cover').attr('src', data.coverPath);
            } else {
                $('#id-audio-cover').css('display', 'none');
            }
        });
        if (currentPage === 'music' || currentPage == 'pictures') {
            musicComponent[0].play();
        }
    }

    function changePicture(data) {
        const pictureComponent = $('#id-picture');
        pictureComponent.attr('src', pictureFilesURL(data.picturePath));
    }

    function showPage(pageName) {
        const previousPage = currentPage;
        currentPage = pageName;
        Object.keys(pageControls).forEach((name) => {
            const page = pageControls[name];
            const isCurrentPage = (name === pageName);
            const display = isCurrentPage ? "block": "None";
            page.controls.forEach((controlId) => {
                $(`#${controlId}`).css("display", display);
            });
            if (page.streamingControl && (previousPage === "videos") || (currentPage === "videos"))
                $(`#${page.streamingControl}`)[0].pause();
        });
    }

    function displayControls(x, y, max_y) {
        const topBarDisplay = y < max_y * 0.25 ? "block" : "none";
        const bottomBarDisplay = y > max_y * 0.75 ? "block" : "none";
        const page = pageControls[currentPage];
        if (page.topBarControl)
            $(`#${page.topBarControl}`).css("display", topBarDisplay);
        if (page.bottomBarControl)
            $(`#${page.bottomBarControl}`).css("display", bottomBarDisplay);
    }

    function handleFrontPanelMouseMove (e) {
        let component = $(this);
        let parent_coords = component.offset();
        let x = e.pageX - parent_coords.left;
        let y = e.pageY - parent_coords.top;

        streamIterationControls.forEach((c) => {
            if (!c.hasClass("buttons-visible")) {
                c.addClass("buttons-visible");
                streamIterationVisibleTimeout = 2;
            }
        });

        displayControls(x, y, component.height());
    }

    const toggleFullScreen = () => {
        (fullScreenTogleFunctions[IsFullScreen() ? 0 : 1]).apply(IsFullScreen() ? doc : de);
    }

    function getCurrentPage() {
        return pageControls[currentPage];
    }

    function handleFrontButtonClicks(event) {
        switch (event) {
            case "next": {
                return function() {
                    getCurrentPage().nextFunction();
                }
            };
            case "previous": {
                return function() {
                    getCurrentPage().previousFunction();
                }
            }
            case "play": {
                return function() {
                    if (getCurrentPage().streamingControl) {
                        const control = $(`#${getCurrentPage().streamingControl}`)[0];
                        if (control.paused)
                            control.play()
                        else
                            control.pause()
                    }
                }
            }
            case "maximize": { return function() {
                    toggleFullScreen();
                }
            }
        }
    }


    function displayVideoAtCurrentIndex() {
        if (currentVideoIdx >= videos.length)
            currentVideoIdx = 0;
        updateVideoInfo(videos[currentVideoIdx])
        changeVideo(videos[currentVideoIdx]);
    }

    function displayPictureAtCurrentIndex() {
        if (currentPictureIdx >= pictures.length)
            currentPictureIdx = 0;
        updatePictureInfo(pictures[currentPictureIdx])
        changePicture(pictures[currentPictureIdx]);
    }

    function displayMusicAtCurrentIndex() {
        if (currentMusicIdx >= musics.length)
            currentMusicIdx = 0;
        updateMusicInfo(musics[currentMusicIdx])
        changeMusic(musics[currentMusicIdx]);
    }

    function sec2time(timeInSeconds) {
        if (Number.isNaN(timeInSeconds))
            return "00:00:00";
        let result = (
            pad = function(num, size) { return ('000' + num).slice(size * -1); }
            , time = Math.round(timeInSeconds)
            , hours = Math.floor(time / 60 / 60)
            , minutes = Math.floor(time / 60) % 60
            , seconds = Math.floor(time - minutes * 60)
            , pad(hours, 2) + ':' + pad(minutes, 2) + ':' + pad(seconds, 2)
        );
        return result;
    }

    (function enableSeekBar() {
        // TODO(Rodrigo): Organize this messy
        const video = $('#id-video')[0];
        const audio = $('#id-audio')[0];
        const seekbarVideo = $('#id-video-seekbar')[0];
        const seekbarAudio = $('#id-audio-seekbar')[0];
        video.addEventListener('ended', () => {nextVideo()}, false);
        audio.addEventListener('ended', () => {nextMusic()}, false);
        video.addEventListener('timeupdate', () => {
            if (!video.seeking) {
                seekbarVideo.value = video.currentTime / video.duration * seekbarVideo.max
            }
            $("#id-video-time").text(sec2time(Math.floor(video.currentTime)) + "/" + sec2time(Math.floor(video.duration))); //Change #current to currentTime
        });
        audio.addEventListener('timeupdate', () => {
            if (!audio.seeking) {
                seekbarAudio.value = audio.currentTime / audio.duration * seekbarVideo.max
            }
            $("#id-audio-time").text(sec2time(Math.floor(audio.currentTime)) + "/" + sec2time(Math.floor(audio.duration))); //Change #current to currentTime
        });
        seekbarVideo.addEventListener('change', () => {
            video.currentTime = video.duration * seekbarVideo.value / seekbarVideo.max;
        });
        seekbarAudio.addEventListener('change', () => {
            audio.currentTime = audio.duration * seekbarAudio.value / seekbarAudio.max;
        });
    }) ();

    (function enablePageClicks() {
        Object.keys(pageControls).forEach((pageName) => {
            $(`#${pageControls[pageName].toggleControl}`).click(() => {
                showPage(pageName);
            });
        });
    }) ();

    (function enableMouseHandling() {
        $("#id-next").click(handleFrontButtonClicks("next"));
        $("#id-pause-play").click(handleFrontButtonClicks("play"));
        $("#id-previous").click(handleFrontButtonClicks("previous"));
        $("#id-maximize").click(handleFrontButtonClicks("maximize"));
        $("#id-front-panel").mousemove(handleFrontPanelMouseMove);
    }) ();

    dePaginator(getVideos) (0, 0, [], function(result) {videos=result; displayVideoAtCurrentIndex();});
    dePaginator(getPictures) (0, 0, [], function(result) {pictures=result; displayPictureAtCurrentIndex();});
    dePaginator(getMusics) (0, 0, [], function(result) {musics=result; displayMusicAtCurrentIndex();});

    showPage("videos");
};

$(document).ready(GalleryModule());
