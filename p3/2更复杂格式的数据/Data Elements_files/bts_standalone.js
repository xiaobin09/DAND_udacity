/**
 * Created by brian on 11/4/16.
 */

jQuery.noConflict();

jQuery( document ).ready(function( $ ) {
    // You can use the locally-scoped $ in here as an alias to jQuery.
    var stickyRibbonTop = $('#header-sticky-wrapper').offset().top;

    $(window).scroll(function(){

        if( $(window).scrollTop() > $('#mode-ribbon-wrapper').height()) {
            $('#header-sticky-wrapper').css({position: 'fixed', top: '0px', width: '100%', zIndex: '5',
                //marginLeft: $('body').css('margin-left'),
                marginRight: $('body').css('margin-right')});
            $('#content').css({'margin-top' : $('#header-sticky-wrapper').height() + "px"})
        } else {
            //$('#header-sticky-wrapper').css({position: 'static', top: '0px'});
            $('#header-sticky-wrapper').css({position: 'static', top: '0px'});
            $('#content').css({'margin-top' : 0})

        }
    });
    // code goes here
});

