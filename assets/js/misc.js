function shareLink(link, title, text, alert_F) {
	if (navigator.share) {
		navigator.share({
			title: title,
			text: text,
			url: link
		})
	} else {
		// Fallback (copy to clipboard)
		navigator.clipboard.writeText(link);
		alert_F('Link copied to clipboard');
	}
}
