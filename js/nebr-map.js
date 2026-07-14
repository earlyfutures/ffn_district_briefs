function showFrozenTooltip(evt, title, sen_name, url1, url2) {
  document.body.classList.add('frozen-active');

  document.querySelectorAll('.selected-area, .selected-label').forEach(function(el) {
    el.classList.remove('selected-area', 'selected-label');
  });

  var dataId = evt.currentTarget.getAttribute('data-id');
  document.querySelectorAll('[data-id="' + dataId + '"]').forEach(function(el) {
    if (el.tagName.toLowerCase() === 'text') {
      el.classList.add('selected-label');
    } else {
      el.classList.add('selected-area');
    }
  });

  var box = document.getElementById('frozen-tooltip');
  if (!box) {
    box = document.createElement('div');
    box.id = 'frozen-tooltip';
    document.body.appendChild(box);
  }
  box.innerHTML = 
    '<span id="close-frozen-tooltip">[X]</span>' +
    '<h2>' + title + '</h2>' +
    '<a href="' + url1 + '" target="_blank">' + sen_name + '</a><br>' +
    '<a href="' + url2 + '" target="_blank">Second URL Link</a>';
  box.style.left = (evt.pageX + 10) + 'px';
  box.style.top  = (evt.pageY + 10) + 'px';
  box.style.display = 'block';
  document.getElementById('close-frozen-tooltip').onclick = function(e) {
    e.stopPropagation();
    hideFrozenTooltip();
  };
  evt.stopPropagation();
}

function hideFrozenTooltip() {
  document.body.classList.remove('frozen-active');
  document.querySelectorAll('.selected-area, .selected-label').forEach(function(el) {
    el.classList.remove('selected-area', 'selected-label');
  });
  var box = document.getElementById('frozen-tooltip');
  if (box) box.style.display = 'none';
}

document.addEventListener('click', function() { hideFrozenTooltip(); });