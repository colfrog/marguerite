let imageContainers = document.getElementsByClassName('image-list');
let draggableItem = null;
let dragStartX = null;
let dragStartY = null;

document.addEventListener('mouseup', dragEnd);

function dragStart(id) {
    draggableItem = document.getElementById(id);
    if (!draggableItem)
        return;

    draggableItem.style.opacity = 0.4;
    document.addEventListener('mousemove', drag);
}

function dragEnd(e) {
    if (!draggableItem)
        return;

    let index = null, category = null;
    // Get the target category
    category = e.target.closest('img').parentNode.parentNode;

    let targetImageList = category.getElementsByClassName('image-list')[0];
    let images = targetImageList.getElementsByTagName('img');
    const x = e.clientX, y = e.clientY;
    for (let i = 0; i < images.length; i++) {
        let rect = images[i].getBoundingClientRect();
        if (images[i] != draggableItem && x < rect.right && x > rect.left && y >= rect.top && y <= rect.bottom) {
            index = i;
        }
    }

    if (index !== null) {
        let j = 0;
        for (let i = 0; i < images.length; i++) {
            if (i == index)
                draggableItem.style.order = j++;
            if (images[i] != draggableItem)
                images[i].style.order = j++;
        }

        let formData = new FormData();
        formData.append("id", draggableItem.id);
        formData.append("pos", draggableItem.style.order);
        formData.append("category", category.id);
        fetch('update-image-position', {
            method: "POST",
            body: formData
        });
    }

    dragStartX = dragStartY = null;
    draggableItem.style.opacity = 1;
    draggableItem.style.transform = null;
    draggableItem = null;
    document.removeEventListener('mousemove', drag);
}

function drag(e) {
    if (!draggableItem)
        return;

    const x = e.clientX;
    const y = e.clientY;

    if (dragStartX === null && dragStartY === null) {
        dragStartX = x;
        dragStartY = y;
    }

    const dx = x - dragStartX;
    const dy = y - dragStartY;

    draggableItem.style.transform = `translate(${dx}px, ${dy}px)`;
}
