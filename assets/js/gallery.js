let current_index = 0;

let videos = [];
let currentSeed = 0;

function dePaginate(page, previousVideos) {
    $.get(`/videos/${currentSeed}/${page}`, function( data ) {
        currentSeed = data.pagination.randomSeed;
        if (data.pagination.page + 1 < data.pagination.pageCount) {
            dePaginate(data.pagination.page + 1, previousVideos.concat(data.items));
        } else {
            videos = previousVideos.concat(data.items);
        }
    });
}

function toggle_random(value, handler) {
   // not implemented yet
}

function change_video(data) {
    let component = $('#id-video');
    component.attr('src', `/files/videos${encodeURI(data.videoPath).replace(/#/g, '%23')}`);

    let content_info = $('#content-info');
    content_info.text(data.videoPath);
    let page_info = $('#page-info');
    page_info.text(`${current_index + 1} of ${videos.length}`);

    try {
        component[0].load();
        component[0].play();
    } catch (error) {
        console.log('---- not user');
    }
}

function next_video() {
    current_index += 1;
    display_current();
}


function prior_video() {
    if (current_index == 0) {
        return;
    }
    current_index -= 1;
    display_current();
}

function displayControls(x, y, max_y) {
    let top_panel = $("#id-top-panel-video");
    let bottom_panel = $("#id-bottom-panel-video");
    if (y < max_y * 0.25) {
        if (top_panel.css('display') !== "block") {
            top_panel.css('display', "block");
        }
    } else {
        if (top_panel.css('display') !== "none") {
            top_panel.css('display', "none");
        }
    }
    if (y > max_y * 0.75) {
        if (bottom_panel.css('display') !== "block") {
            bottom_panel.css('display', "block");
        }
    } else {
        if (bottom_panel.css('display') !== "none") {
            bottom_panel.css('display', "none");
        }
    }
}

function mouse_move_handler (e) {
    let component = $(this);
    let parent_coords = component.offset();
    let x = e.pageX - parent_coords.left;
    let y = e.pageY - parent_coords.top;

    displayControls(x, y, component.height());
}

function goFullScreen() {
    let element = document.body;
    // Supports most browsers and their versions.
    let requestMethod = element.requestFullScreen || element.webkitRequestFullScreen || element.mozRequestFullScreen || element.msRequestFullScreen;

    if (requestMethod) { // Native full screen.
        requestMethod.call(element);
    } else if (typeof window.ActiveXObject !== "undefined") { // Older IE.
        let wscript = new ActiveXObject("WScript.Shell");
        if (wscript !== null) {
            wscript.SendKeys("{F11}");
        }
    }
}

function viewer_click(e) {
    let component = $(this);
    let parent_coords = component.offset();
    let x = e.pageX - parent_coords.left;
    let y = e.pageY - parent_coords.top;
    let video = $("#id-video")[0];

    if (y >= component.height() - 85) {
        return;
    }

    if (y <= component.height() / 4 && x >= component.width() - component.width() / 4) {
        e.preventDefault();
        goFullScreen();
        return;
    }

    if (x > (component.width() / 3) * 2) {
        e.preventDefault();
        next_video();
    } else if (x < component.width() / 3) {
        e.preventDefault();
        prior_video();
    } else {
        if (video.paused)
            video.play();
        else
            video.pause();
    }
}

function display_current() {
    if (current_index >= videos.length) {
        current_index = 0;
    }
    if (videos.length < 1) {
        setTimeout(display_current, 2000);
    } else {
        change_video(videos[current_index]);
    }
}

function sec2time(timeInSeconds) {
    var pad = function(num, size) { return ('000' + num).slice(size * -1); },
    time = parseFloat(timeInSeconds).toFixed(3),
    hours = Math.floor(time / 60 / 60),
    minutes = Math.floor(time / 60) % 60,
    seconds = Math.floor(time - minutes * 60),
    milliseconds = time.slice(-3);

    return pad(hours, 2) + ':' + pad(minutes, 2) + ':' + pad(seconds, 2);
}

function EnableSeekBar() {
    const video = $('#id-video')[0];
    const seekbar = $('#id-seekbar')[0];

    video.addEventListener('timeupdate', () => {
        if (!video.seeking) {
            seekbar.value = video.currentTime / video.duration * seekbar.max
        }
        $("#id-current-time").text(sec2time(Math.floor(video.currentTime)) + "/" + sec2time(Math.floor(video.duration))); //Change #current to currentTime
    });

    seekbar.addEventListener('change', () => {
        video.currentTime = video.duration * seekbar.value / seekbar.max;
    });
}

$(document).ready(function(){
    EnableSeekBar();
    $("#id-videos-div").mouseup(viewer_click);
    $("#id-videos-div").mousemove(mouse_move_handler);
    videos = []
    dePaginate(0, []);
    display_current();
});
