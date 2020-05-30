function GalleryModule() {
    let currentVideoIdx = 0;
    let currentPictureIdx = 0;
    let currentMusicIdx = 0;
    let updatingContent = false;
    let videos = [];
    let pictures = [];
    let musics = [];
    let currentPage = "videos";

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

    function hasAttr(element, name) {
        const attr = element.attr(name);
        if (typeof attr !== typeof undefined && attr !== false) {
            return true;
        }
        if (name.startsWith('data-')) {
            const data = element.data(name.substr(5));
            return (typeof data !== typeof undefined && data !== false);
        }
        return false;
    }

    const streamIterationControls = [
        $("#id-previous"),
        $("#id-next"),
        $("#id-pause-play"),
        $("#id-maximize"),
        $("#id-repeat"),
        $("#id-random"),
        $("#id-search"),
    ]

    const pageControls = {
        "videos": {
            "controls": ["id-video-page", "id-bottom-panel-video"],
            "order": 0,
            "toggleControl": "id-toggle-video",
            "streamingControl": $("#id-video"),
            "topBarControl": "id-top-panel",
            "bottomBarControl": "id-bottom-panel-video",
            "nextFunction": nextVideo,
            "previousFunction": previousVideo,
            "streamControls": streamIterationControls,
            "repeatControl": $("#id-repeat"),
            "filter": "",
        },
        "music": {
            "controls": ["id-audio-page", "id-bottom-panel-music"],
            "order": 1,
            "toggleControl": "id-toggle-music",
            "topBarControl": "id-top-panel",
            "bottomBarControl": "id-bottom-panel-music",
            "streamingControl": $("#id-audio"),
            "nextFunction": nextMusic,
            "previousFunction": previousMusic,
            "streamControls": streamIterationControls,
            "repeatControl": $("#id-repeat"),
            "filter": "",
        },
        "pictures": {
            "controls": ["id-picture-page", "id-bottom-panel-picture"],
            "order": 1,
            "topBarControl": "id-top-panel",
            "toggleControl": "id-toggle-picture",
            "nextFunction": nextPicture,
            "streamingControl": $("#id-audio"),
            "previousFunction": previousPicture,
            "bottomBarControl": "id-bottom-panel-picture",
            "streamControls": streamIterationControls.filter((c) => (c.attr('id') !== "id-repeat")),
            "filter": "",
        }
    };

    let streamIterationVisibleTimeout = 2;

    setInterval(() => {
        if (streamIterationVisibleTimeout > 0) {
            streamIterationVisibleTimeout -= 1;
        } else if (currentPage) {
            pageControls[currentPage].streamControls
                .forEach((c) => { c.removeClass("buttons-visible");});
            if (getCurrentPage().repeatControl) {
                getCurrentPage().repeatControl.removeClass('repeat-on');
            }
            $("#id-random").removeClass('random-on');
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
            const args = Array.from(arguments).slice(0, -2);
            const filter = urlEncode(Array.from(arguments).slice(-2)[0]);
            const callback = Array.from(arguments).slice(-1)[0];
            const url = format.apply(template, args);
            if (filter) {
                $.get(`${url}?filter=${urlEncode(filter)}`, callback);
            } else {
                $.get(url, callback);
            }
        }
    }

    function urlEncode(content) {
        return encodeURI(content).replace(/#/g, '%23');
    }

    function composeURI(base) {
        return function(path) {
            return `${base}${urlEncode(path)}`
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
        return function dePaginate(currentSeed, page, filter, currentList, resultCb) {
            getFunction(currentSeed, page, filter, function(data) {
                if (data.pagination.page + 1 < data.pagination.pageCount)
                    dePaginate(data.pagination.randomSeed, data.pagination.page + 1, filter, currentList.concat(data.items), resultCb);
                else {
                    updatingContent = false;
                    $("#id-filter-panel").css('display', 'none');
                    resultCb(currentList.concat(data.items));
                }
            });
        };
    }

    function updateVideoInfo(data) {
        $('#content-info').text(data ? data.videoPath : "No Content");
        $('#id-video-page-info').text(`${currentVideoIdx + 1} of ${videos.length} videos`);
        $('#id-video-seekbar')[0].time = 0;
    }

    function updateMusicInfo(data) {
        $('#content-info-music').text(data ? data.musicPath : "No Content");
        $('#id-audio-page-info').text(`${currentMusicIdx + 1} of ${musics.length} songs`);
        $('#id-audio-seekbar')[0].time = 0;
    }

    function updatePictureInfo(data) {
        $("#id-path-picture").text(data ? data.picturePath : "No Content");
        $("#id-count-picture").text(`${currentPictureIdx + 1} of ${pictures.length} pictures`);
    }

    function changeVideo(data) {
        const videoComponent = $('#id-video');
        videoComponent.attr('src', data ? videoFilesURL(data.videoPath) : "");
        videoComponent[0].load();
        if (currentPage === 'videos') {
            videoComponent[0].play();
        }
    }

    function changeMusic(data) {
        const musicComponent = $('#id-audio');
        musicComponent.attr('src', data ? musicFilesURL(data.musicPath) : "");
        if (data) {
            getCover(data.musicPath, "", (data)=> {
                if (data.coverPath) {
                    $('#id-audio-cover').css('display', 'block');
                    $('#id-audio-cover').attr('src', data.coverPath);
                } else {
                    $('#id-audio-cover').css('display', 'none');
                }
            });
        } else {
            $('#id-audio-cover').css('display', 'none');
        }
        if (currentPage === 'music' || currentPage == 'pictures') {
            musicComponent[0].play();
        }
    }

    function getPageControl() {
        const page = pageControls[currentPage];
        return currentPage === 'pictures' ? $("#id-picture") : page.streamingControl
    }

    function changePicture(data) {
        const pictureComponent = $('#id-picture');
        pictureComponent.attr('src', data ? pictureFilesURL(data.picturePath) : "");
    }

    function showRepeat() {
        const page = pageControls[currentPage];
        if (page.streamingControl && page.repeatControl) {
            page.repeatControl.removeClass('repeat-on');
            if (hasAttr(page.streamingControl, 'loop')) {
                page.repeatControl.addClass('repeat-on');
            }
        }
    }

    function showRandom() {
        const page = pageControls[currentPage];
        const control = getPageControl();
        const randomControl = $("#id-random");
        randomControl.removeClass('random-on');
        if (hasAttr(control, 'data-random')) {
            randomControl.addClass('random-on');
        }
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
                            page.streamingControl[0].pause();
        });
        if (previousPage)
            pageControls[previousPage].streamControls.forEach((c) => {c.css('display', 'none')});
        pageControls[currentPage].streamControls.forEach((c) => {c.css('display', 'block')});
        $("#id-filter-editor").val(pageControls[currentPage].filter);
        showRepeat();
        showRandom();
        saveSettings();
    }

    function loadSettings() {
        const rawSetting = localStorage.getItem('hask-gallery');
        if (!rawSetting) {
            return;
        }
        const settings = JSON.parse(rawSetting);
        if (settings.videoRepeat) {
            $("#id-video").attr('loop', "1");
        }
        if (settings.musicRepeat) {
            $("#id-audio").attr('loop', "1");
        }
        if (settings.videoRandom) {
            $("#id-video").data('random', "1");
        }
        if (settings.musicRandom) {
            $("#id-audio").data('random', "1");
        }
        if (settings.pictureRandom) {
            $("#id-picture").data('random', "1");
        }

        pageControls["videos"].filter = settings.videosFilter || "";
        pageControls["pictures"].filter = settings.picturesFilter || "";
        pageControls["music"].filter = settings.musicsFilter || "";

        currentPage = settings.currentPage || "videos";
    }

    function saveSettings() {
        const settings = {
            videoRandom: hasAttr($("#id-video"), 'data-random'),
            videoRepeat: hasAttr($("#id-video"), 'loop'),
            musicRepeat: hasAttr($("#id-audio"), 'loop'),
            musicRandom: hasAttr($("#id-audio"), 'data-random'),
            pictureRandom: hasAttr($("#id-picture"), 'data-random'),
            videosFilter: pageControls["videos"].filter,
            picturesFilter: pageControls["pictures"].filter,
            musicsFilter: pageControls["music"].filter,
            currentPage,
        }
        localStorage.setItem('hask-gallery', JSON.stringify(settings));
    }

    function getInitialSeed(control) {
        if (hasAttr(control, 'data-random')) {
            return 0;
        }
        return 1;
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
                showRepeat();
                showRandom();
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

    function reloadPageContents(thePage) {
        const page = pageControls[thePage];
        if (thePage === 'videos') {
            currentVideoIdx = 0;
            dePaginator(getVideos) (getInitialSeed($('#id-video')), 0, page.filter, [], function(result) {videos=result; displayVideoAtCurrentIndex();});
        } else if (thePage === 'pictures') {
            currentPictureIdx = 0;
            dePaginator(getPictures) (getInitialSeed($('#id-picture')), 0, page.filter, [], function(result) {pictures=result; displayPictureAtCurrentIndex();});
        } else {
            currentMusicIdx = 0;
            dePaginator(getMusics) (getInitialSeed($('#id-audio')), 0, page.filter, [], function(result) {musics=result; displayMusicAtCurrentIndex();});
        }
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
                        const control = getCurrentPage().streamingControl[0];
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
            case "repeat": {
                return function() {
                    if (getCurrentPage().streamingControl) {
                        const player = getCurrentPage().streamingControl;
                        if (hasAttr(player, 'loop')) {
                            player.attr('loop', false);
                        } else {
                            player.attr('loop', '1');
                        }
                        showRepeat();
                        saveSettings();
                    }
                }
            }
            case "random": {
                return function() {
                    if (updatingContent) {
                        return;
                    }
                    updatingContent = true;
                    const page = pageControls[currentPage];
                    const control = getPageControl();
                    if (hasAttr(control, 'data-random')) {
                        control.data('random', false);
                    } else {
                        control.data('random', '1');
                    }
                    showRandom();
                    saveSettings();
                    reloadPageContents(currentPage);
                }
            }
            case "search": {
                return function() {
                    if ($("#id-filter-panel").css('display') !== "block") {
                        $("#id-filter-panel").css('display', 'block');
                    } else {
                        $("#id-filter-panel").css('display', 'none');
                    }
                }
            }
            case "apply-search": {
                return function() {
                    if (updatingContent) {
                        return;
                    }
                    updatingContent = true;
                    const page = pageControls[currentPage];
                    page.filter = $("#id-filter-editor").val();
                    saveSettings();
                    reloadPageContents(currentPage);
                }
            }
        }
    }

    function displayVideoAtCurrentIndex() {
        if (currentVideoIdx >= videos.length)
            currentVideoIdx = 0;
        if (videos.length === 0) {
            updateVideoInfo(undefined);
            changeVideo(undefined);
            return;
        }
        updateVideoInfo(videos[currentVideoIdx])
        changeVideo(videos[currentVideoIdx]);
    }

    function displayPictureAtCurrentIndex() {
        if (currentPictureIdx >= pictures.length)
            currentPictureIdx = 0;
        if (pictures.length === 0) {
            updatePictureInfo(undefined);
            changePicture(undefined);
            return;
        }
        updatePictureInfo(pictures[currentPictureIdx])
        changePicture(pictures[currentPictureIdx]);
    }

    function displayMusicAtCurrentIndex() {
        if (currentMusicIdx >= musics.length)
            currentMusicIdx = 0;
        if (musics.length === 0) {
            updateMusicInfo(undefined);
            changeMusic(undefined);
            return;
        }
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
        let inSeek = false;
        video.addEventListener('timeupdate', () => {
            if (!inSeek) {
                seekbarVideo.value = video.currentTime / video.duration * seekbarVideo.max
            }
            $("#id-video-time").text(sec2time(Math.floor(video.currentTime)) + "/" + sec2time(Math.floor(video.duration))); //Change #current to currentTime
        });
        audio.addEventListener('timeupdate', () => {
            if (!inSeek) {
                seekbarAudio.value = audio.currentTime / audio.duration * seekbarVideo.max
            }
            $("#id-audio-time").text(sec2time(Math.floor(audio.currentTime)) + "/" + sec2time(Math.floor(audio.duration))); //Change #current to currentTime
        });
        seekbarVideo.addEventListener('change', () => {
            inSeek = true;
            video.currentTime = video.duration * seekbarVideo.value / seekbarVideo.max;
            inSeek = false;
        });
        seekbarAudio.addEventListener('change', () => {
            inSeek = true;
            audio.currentTime = audio.duration * seekbarAudio.value / seekbarAudio.max;
            inSeek = false;
        });

        $(seekbarAudio).mousedown(() => {
            inSeek = true;
        });

        $(seekbarVideo).mousedown(() => {
            inSeek = true;
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
        $("#id-repeat").click(handleFrontButtonClicks("repeat"));
        $("#id-random").click(handleFrontButtonClicks("random"));
        $("#id-search").click(handleFrontButtonClicks("search"));
        $("#id-filter-button").click(handleFrontButtonClicks("apply-search"));
        $("#id-front-panel").mousemove(handleFrontPanelMouseMove);
    }) ();

    loadSettings();

    reloadPageContents("videos");
    reloadPageContents("pictures");
    reloadPageContents("music");

    showPage(currentPage);
};

$(document).ready(GalleryModule());
