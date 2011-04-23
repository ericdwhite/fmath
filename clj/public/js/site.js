function setupToggles() {
    jQuery(".boxhead .trigger").click(function() {
        var boxhead = $(this).parent('.boxhead');
        var current = $(this).attr('alt');
        if ( current.indexOf('collapse')!=-1 ) {
            boxhead.addClass('bh-collapsed');
            boxhead.removeClass('bh-expanded');
            $(this).attr({alt: 'expand',
                         src: './theme/box-expand.png'});
        } else {
            boxhead.addClass('bh-expanded');
            boxhead.removeClass('bh-collapsed');
            $(this).attr({alt: 'collapse',
                         src: './theme/box-collapse.png'});
        }
        var content = boxhead.next(".boxcontent");
        content.slideToggle(150);
    });
}

function configurejQPlot() {                
    $.jqplot.config.enablePlugins = true;
}

function chartRandomWalk(d1, d2, d3) {
    $('#chartdiv').empty();
    plot = $.jqplot('chartdiv',
                    [
                        d1, d2, d3
                    ],
                    {
                        title: "3 Random walks starting at 50",
                        cursor:{zoom:true, showTooltip:false},
                        axes: {xaxis:{min:0, max:110}, yaxis:{min: 0, max:100}},
                        series: [{color: '#ffa500'},{lineWidth: 0.5}]
                    } );
} 

function configureRandomWalk() {
    var data1;
    var data2;
    var data3;
    $.getJSON('math/randomwalk/', function(data) {
        data1 = data;
        $.getJSON('math/randomwalk/', function(data) {
            data2 = data;
            $.getJSON('math/randomwalk/', function(data) {
                data3 = data;
                chartRandomWalk(data1, data2, data3);
            });
        });
    });
}
