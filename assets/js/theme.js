function setTheme(theme) {
	if (theme == 'dark') {
		document.documentElement.setAttribute('data-theme', 'dark');
		document.body.style.colorScheme = 'dark';
	} else {
		document.documentElement.removeAttribute('data-theme');
		document.body.style.colorScheme = 'light';
	}
	localStorage.setItem('theme', theme);
}

function getTheme() {
	const saved = localStorage.getItem('theme')
	const isDark = !saved
		? window.matchMedia('(prefers-color-scheme: dark)').matches
		: saved == 'dark'
	if (isDark)
		return 'dark';
	return 'light';
}

function initTheme() {
	if (getTheme() == 'dark')
		document.documentElement.setAttribute('data-theme', 'dark');
	else
		document.documentElement.removeAttribute('data-theme');

	function _initial_theme_scheme() {
		window.removeEventListener('load', _initial_theme_scheme);
		setTheme(getTheme());
	}
	window.addEventListener('load', _initial_theme_scheme);
}
