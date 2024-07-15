const months = ['Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun', 'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez'];
const date = new Date();

function render() {
  let output = '';

  const thisMonth = months[date.getMonth()];

  for (let month of months) {
    const active = thisMonth == month ? 'active' : '';
    // output = output + '<div>' + month + '</div>'
    output += `<div class="${active}">${month}</div>` //versão otimizada do código acima
  }

  return output;
}

document.querySelector('main').innerHTML = render();
document.querySelector('header span').innerHTML = date.getFullYear();




function getDaysOfMonth(){ 
  return new Date(date.getFullYear(), date.getMonth()+1, 0).getDate();
}

function renderDaysOfMonth(){
  let output = '';
  
  const lastDay = getDaysOfMonth();
  let day = 1;
  
  for(day; day <= lastDay; day++){
    const active = date.getDate() == day ? 'active' : '';
    output += `<div class="${active}">${day}</div>`;
  }
  
  return output;
}

const element = document.getElementById("dayMonth");
element.querySelector('main').innerHTML = renderDaysOfMonth();
element.querySelector('header span').innerHTML = `${date.toLocaleString('pt', { month: 'long' }).toUpperCase()} ${date.getFullYear()}`;