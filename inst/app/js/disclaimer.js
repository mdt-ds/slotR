// script for highlighting the footer disclaimer on mouseover
const dp = document.querySelectorAll(".disclaimer");
console.log(dp);
let i;
for (i = 0; i < dp.length; i++) {
    dp[i].addEventListener('mouseover', see);
    dp[i].addEventListener('mouseout', unsee);
    }

function see(e) {
    let i;
    for (i = 0; i < dp.length; i++) {
    dp[i].style.color = 'red';
    dp[i].style.fontSize = "16px";
    }
}

function unsee(e) {
    let i;
    for (i = 0; i < dp.length; i++) {
    dp[i].style.color = 'white';
    dp[i].style.fontSize = "8px";
    }    
}