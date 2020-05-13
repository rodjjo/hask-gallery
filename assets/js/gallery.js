function GalleryModule() {
    let currentIndex = 0;
    let videos = [];
    let currentPage = "";

    const pageControls = {
        "videos": {
            "controls": ["id-video-page", "id-bottom-panel-video"],
            "order": 0,
            "toggleControl": "id-toggle-video",
            "streamingControl": "id-video",
            "topBarControl": "id-top-panel",
            "bottomBarControl": "id-bottom-panel-video",
        },
        "music": {
            "controls": ["id-music-page", "id-bottom-panel-music"],
            "order": 1,
            "toggleControl": "id-toggle-music",
            "topBarControl": "id-top-panel",
            "bottomBarControl": "id-bottom-panel-video",
        },
        "pictures": {
            "controls": ["id-picture-page", "id-bottom-panel-picture"],
            "order": 1,
            "topBarControl": "id-top-panel",
            "toggleControl": "id-toggle-picture",
        }
    };

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
    const songsFilesURL = composeURI('/files/songs');
    const pictureFilesURL = composeURI('/files/pictures');

    const getVideos = get('/videos/{0}/{1}/');
    const getSongs = get('/songs/{0}/{1}/');
    const getPictures = get('/pictures/{0}/{1}/');

    function updateVideoInfo(data) {
        $('#content-info').text(data.videoPath);
        $('#id-video-page-info').text(`${currentIndex + 1} of ${videos.length} videos`);
        $('#id-video-seekbar')[0].time = 0;
    }

    function changeVideo(data) {
        const videoComponent = $('#id-video');
        videoComponent.attr('src', videoFilesURL(data.videoPath));
        videoComponent[0].load();
        videoComponent[0].play();
    }

    function nextVideo() {
        currentIndex += 1;
        displayVideoAtCurrentIndex();
    }


    function previousVideo() {
        if (currentIndex > 0) {
            currentIndex -= 1;
            displayVideoAtCurrentIndex();
        }
    }

    function showPage(pageName) {
        currentPage = pageName;
        Object.keys(pageControls).forEach((name) => {
            const page = pageControls[name];
            const isCurrentPage = (name === pageName);
            const display = isCurrentPage ? "block": "None";
            const streamingFunction = isCurrentPage ? "play" : "pause";
            page.controls.forEach((controlId) => {
                $(`#${controlId}`).css("display", display);
            });
            if (page.streamingControl)
                ($(`#${page.streamingControl}`)[0][streamingFunction])();
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

        displayControls(x, y, component.height());
    }

    const toggleFullScreen = () => {
        (fullScreenTogleFunctions[IsFullScreen() ? 0 : 1]).apply(IsFullScreen() ? doc : de);
    }

    function handleFrontPanelClick(e) {
        const page = pageControls[currentPage];
        const component = $(this);
        const parentCoords = component.offset();
        const x = e.pageX - parentCoords.left;
        const y = e.pageY - parentCoords.top;
        const maxClickableY = page.bottomBarControl ? $( window ).height() - $(`#${page.bottomBarControl}`).height() : 1000000000;
        const minClickableY = page.topBarControl ? $(`#${page.topBarControl}`).height() : 0;

        if (maxClickableY < y || y < minClickableY)
            return;

        e.preventDefault();

        if (y <= component.height() / 4 && x >= component.width() - component.width() / 4) {
            toggleFullScreen();
            return;
        }

        if (currentPage === "videos") {
            let video = $("#id-video")[0];
            if (x > (component.width() / 3) * 2) {
                nextVideo();
            } else if (x < component.width() / 3) {
                previousVideo();
            } else {
                if (video.paused)
                    video.play();
                else
                    video.pause();
            }
        }
    }

    function validateCurrentIndex() {
        if (currentIndex >= videos.length)
            currentIndex = 0;
    }

    function displayVideoAtCurrentIndex() {
        validateCurrentIndex();
        updateVideoInfo(videos[currentIndex])
        changeVideo(videos[currentIndex]);
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
        const video = $('#id-video')[0];
        const seekbar = $('#id-video-seekbar')[0];
        video.addEventListener('ended', () => {nextVideo()}, false);
        video.addEventListener('timeupdate', () => {
            if (!video.seeking) {
                seekbar.value = video.currentTime / video.duration * seekbar.max
            }
            $("#id-video-time").text(sec2time(Math.floor(video.currentTime)) + "/" + sec2time(Math.floor(video.duration))); //Change #current to currentTime
        });
        seekbar.addEventListener('change', () => {
            video.currentTime = video.duration * seekbar.value / seekbar.max;
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
        $("#id-front-panel").mouseup(handleFrontPanelClick);
        $("#id-front-panel").mousemove(handleFrontPanelMouseMove);
    }) ();

    (function dePaginate(currentSeed, page, currentList, resultCb) {
        getVideos(currentSeed, page, function(data) {
            if (data.pagination.page + 1 < data.pagination.pageCount)
                dePaginate(data.pagination.randomSeed, data.pagination.page + 1, currentList.concat(data.items), resultCb);
            else
                resultCb(currentList.concat(data.items))
        });
    }) (0, 0, [], function(result) {videos=result; displayVideoAtCurrentIndex();});

    showPage("videos");
};

$(document).ready(GalleryModule());
