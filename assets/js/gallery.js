var current_index = 0;

var videos = [];
var currentSeed = 0;

function dePaginate(page, previousVideos) {
    $.get( `videos/${currentSeed}/${page}`, function( data ) {
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
    var component = $('#video-viewer');
    component.attr('src', `/files/videos${encodeURI(data.videoPath).replace(/#/g, '%23')}`);

    try {
        component[0].play();
    } catch (error) {
        console.log('---- not user');
    }

    var content_info = $('#content-info');
    content_info.text(data.videoPath);
    var page_info = $('#page-info');
    page_info.text(`${current_index + 1} of ${videos.length}`);
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

function viewer_click(e) {
    let component = $(this);
    let parent_coords = component.parent().offset();
    let x = e.pageX - parent_coords.left;
    let y = e.pageY - parent_coords.top;

    if (y <= component.height() / 4 && x >= component.width() - component.width() / 4) {
        e.preventDefault();
        let video = $("#video-viewer")[0];
        if (video.requestFullscreen) {
            video.requestFullscreen();
        } else if (video.msRequestFullscreen) {
            video.msRequestFullscreen();
        } else if (video.mozRequestFullScreen) {
            video.mozRequestFullScreen();
        } else if (video.webkitRequestFullscreen) {
            video.webkitRequestFullscreen();
        }
        return;
    }

    if (x > (component.width() / 3) * 2) {
        e.preventDefault();
        next_video();
    } else if (x < component.width() / 3) {
        e.preventDefault();
        prior_video();
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


function toggle_changed() {
    toggle_random($("#toggle-random").prop('checked'), function(data) {
        $("#toggle-random").change(toggle_changed);
        display_current();
    })
}

$(document).ready(function(){
    $("#video-viewer").mouseup(viewer_click);
    $("#toggle-random").change(toggle_changed);
    videos = []
    dePaginate(0, []);
    display_current();
});
