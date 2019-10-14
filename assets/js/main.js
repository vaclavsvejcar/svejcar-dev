
$(function () {

    $('#toggle-sidebar').on('click', function (ev) {
        $('aside').toggleClass('visible');
    });

    $('div.post-content p a').each(function (i) {
        $(this).attr('target', '_blank');
    });

    $('.post-content').children('h1, h2, h3, h4, h5').each(function () {
        var id = $(this).attr('id');
        var text = $(this).html();

        $(this)
            .html('')
            .append('<a href="#' + id + '" class="header-link">' + text + '</a>');
    });
});