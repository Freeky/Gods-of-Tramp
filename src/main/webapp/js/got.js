// Google Analytics
var _gaq = _gaq || [];
_gaq.push([ '_setAccount', 'UA-24226649-1' ]);
_gaq.push([ '_trackPageview' ]);
_gaq.push([ 'b._setAccount', 'UA-29177726-1' ]);
_gaq.push([ 'b._trackPageview' ]);

(function() {
	var ga = document.createElement('script');
	ga.type = 'text/javascript';
	ga.async = true;
	ga.src = ('https:' == document.location.protocol ? 'https://ssl'
			: 'http://www')
			+ '.google-analytics.com/ga.js';
	var s = document.getElementsByTagName('script')[0];
	s.parentNode.insertBefore(ga, s);
})();

// Facebook Panel
(function(d, s, id) {
	var js, fjs = d.getElementsByTagName(s)[0];
	if (d.getElementById(id))
		return;
	js = d.createElement(s);
	js.id = id;
	js.src = "//connect.facebook.net/de_DE/all.js#xfbml=1";
	fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));

// Submenu
$('.sidebar ul li').hover(function() {
	$(this).find('ul').show();
	}, function() {
	$(this).find('ul').hide();
});