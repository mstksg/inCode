var source_toggled = false;

$(document).ready(function() {
    $('#toc').toc({
        'selectors': 'h1,h2,h3,h4', //elements to use as headings
        'container': '.main-content', //element to find all selectors in
        'smoothScrolling': true, //enable or disable smooth scrolling on click
        'prefix': 'sec', //prefix for anchor tags and class names
        // 'onHighlight': function(el) {}, //called when a new section is highlighted
        'highlightOnScroll': false, //add class to heading that is currently in focus
        // 'highlightOffset': 100, //offset to trigger the next headline
        'anchorName': function(i, heading, prefix) { //custom function for anchor name
            return prefix+i;
        },
        'headerText': function(i, heading, $heading) { //custom function building the header-item text
            return $heading.text();
        },
        'itemClass': function(i, heading, $heading, prefix) { // custom function for item class
            return $heading[0].tagName.toLowerCase();
        }
    });


    $('.main-content').children('h2,h3,h4,h5').append('<a href="#title" class="top-link">top</a>');

    $('.top-link').click(function(e) {
        scrollTo(e);
    });

    if ($('#toc li').length > 0) {
        $('.contents-container').show();
    }


    if ($('.source-info').length > 0) {
        $('.article header').hover(function() {
            if (!source_toggled) {
                $('.source-info').show();
            }
        }, function() {
            if (!source_toggled) {
                $('.source-info').hide();
            }
        }).click(function() {
            if (source_toggled) {
                $('.source-info').hide();
            } else {
                $('.source-info').show();
            }
            source_toggled = !source_toggled;
        });
    }



});


var scrollTo = function(e) {
    e.preventDefault();
    var elScrollTo = $(e.target).attr('href');
    var $el = $(elScrollTo);

    $('body,html').animate({ scrollTop: $el.offset().top }, 400, 'swing', function() {
        location.hash = elScrollTo;
    });
};

