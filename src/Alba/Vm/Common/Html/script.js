// Copyright (c) 2025 albaDsl

function getChildren(node) {
    return document.querySelectorAll(`[data-path^="${node.dataset.path + "."}"]`);
}

function hideChildren(node) {
    const c = getChildren(node);
    c.forEach(el => el.style.display = 'none');
}

function toggleNode(node) {
    node.classList.toggle('collapsed');
    if (node.classList.contains('collapsed') === true) {
        node.querySelector('.col-stack').textContent =
            node.querySelector('.stackSummary').textContent;
    } else {
        node.querySelector('.col-stack').textContent =
            node.querySelector('.stack').textContent;
    }

    const elements = getChildren(node);
    console.log(elements)

    if (node.classList.contains('collapsed') === true) {
        elements.forEach(el => el.style.display = 'none');
    }
    else {
        elements.forEach(el => el.style.display = 'grid');
        const collapsed = document.querySelectorAll(`.collapsed[data-path^="${node.dataset.path + "."}"]`);
        collapsed.forEach(el => hideChildren(el));
        console.log(collapsed);
    }
}

document.addEventListener('DOMContentLoaded', () => {
    const parents = document.querySelectorAll('#log-entries > div.has-children');

    parents.forEach(div => {
        div.addEventListener('click', function(event) {toggleNode(this);});
        toggleNode(div);
    });

});
