/*jslint browser */
/*jslint es6:true*/

(function () {
    'use strict';

    const toggleNav = function () {
        document.querySelector('#toggle-sidebar').onclick = function () {
            document.querySelector('aside').classList.toggle('visible');
        };
    };

    const linksInNewTab = function () {
        const linkList = document.querySelectorAll('div.post-content a');
        for (let i in linkList) {
            linkList[i].setAttribute('target', '_blank');
        }
    };

    toggleNav();
    linksInNewTab();

}());