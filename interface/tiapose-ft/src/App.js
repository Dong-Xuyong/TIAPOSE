import './App.css';
import { useState, useEffect } from 'react';
import Chart from "chart.js/auto";
import { Line } from 'react-chartjs-2';
function App() {
  function splitArrayByPattern(arr) {
    const budv1 = [];
    const budv2 = [];
    const budv3 = [];
    const stellav1 = [];
    const stellav2 = [];
    const stellav3 = [];
    for (let i = 1; i < (arr.length / 6); i++) {
      const index = ((6 * (i - 1)) + 1);
      budv1.push(arr[index - 1]);
      budv2.push(arr[index]);
      budv3.push(arr[index + 1]);
      stellav1.push(arr[index + 2]);
      stellav2.push(arr[index + 3]);
      stellav3.push(arr[index + 4]);
    }
    return [budv1, budv2, budv3, stellav1, stellav2, stellav3];
  }

  function splitArrayAlternating(arr) {
    const bud = [];
    const stella = [];
    for (let i = 1; i < (arr.length / 2); i++) {
      const index = ((2 * (i - 1)) + 1);
      bud.push(arr[index - 1]);
      stella.push(arr[index]);
    }
    return [bud, stella];
  }
  /* GRAPHS DATA */
  const [procuraDataStella, setProcuraDataStella] = useState([]);
  const [procuraDataBud, setProcuraDataBud] = useState([]);
  const [retalhista, setRetalhista] = useState([]);
  const [v1, setV1] = useState([]);
  const [v2, setV2] = useState([]);
  const [v3, setV3] = useState([]);
  const [quantidade_bud_v1, setQtyBudV1] = useState([]);
  const [quantidade_bud_v2, setQtyBudV2] = useState([]);
  const [quantidade_bud_v3, setQtyBudV3] = useState([]);
  const [quantidade_stella_v1, setQtyStellaV1] = useState([]);
  const [quantidade_stella_v2, setQtyStellaV2] = useState([]);
  const [quantidade_stella_v3, setQtyStellaV3] = useState([]);
  const [sobras_bud, setSobrasBud] = useState([]);
  const [sobras_Stella, setSobrasStella] = useState([]);
  const [quantidade_bud, setQtyBud] = useState([]);
  const [quantidade_stella, setQtyStella] = useState([]);
  const [evaluation, setEvaluation] = useState(0);
  /* GRAPHS ITSELF */
  /* THE LABEL */
  const labels = ["1", "2", "3", "4", "5", "6", "7"];
  const graficoProcura = {
    labels: labels,
    datasets: [
      {
        label: "Stella",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: procuraDataStella,
      },
      {
        label: "Bud",
        backgroundColor: "rgb(255, 20, 132)",
        borderColor: "rgb(255, 20, 132)",
        data: procuraDataBud,
      }
    ],
  };

  const graficoRetalhista = {
    labels: labels,
    datasets: [
      {
        label: "Armazem",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: retalhista,
      }
    ],
  };

  const graficoV1 = {
    labels: labels,
    datasets: [
      {
        label: "v1",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: v1,
      }
    ],
  };

  const graficoV2 = {
    labels: labels,
    datasets: [
      {
        label: "v2",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: v2,
      }
    ],
  };

  const graficoV3 = {
    labels: labels,
    datasets: [
      {
        label: "v3",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: v3,
      }
    ],
  };

  const graficoQBudv1 = {
    labels: labels,
    datasets: [
      {
        label: "Quantidade Bud V1",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: quantidade_bud_v1,
      }
    ],
  };

  const graficoQBudv2 = {
    labels: labels,
    datasets: [
      {
        label: "Quantidade Bud V2",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: quantidade_bud_v2,
      }
    ],
  };

  const graficoQBudv3 = {
    labels: labels,
    datasets: [
      {
        label: "Quantidade Bud V3",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: quantidade_bud_v3,
      }
    ],
  };

  const graficoQStellav1 = {
    labels: labels,
    datasets: [
      {
        label: "Quantidade Stella V1",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: quantidade_stella_v1,
      }
    ],
  };

  const graficoQStellav2 = {
    labels: labels,
    datasets: [
      {
        label: "Quantidade Stella V2",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: quantidade_stella_v2,
      }
    ],
  };

  const graficoQStellav3 = {
    labels: labels,
    datasets: [
      {
        label: "Quantidade Stella V3",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: quantidade_stella_v3,
      }
    ],
  };

  const graficoSobrasBud = {
    labels: labels,
    datasets: [
      {
        label: "Sobras Bud",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: sobras_bud,
      }
    ],
  };

  const graficoSobrasStella = {
    labels: labels,
    datasets: [
      {
        label: "Sobras Stella",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: sobras_Stella,
      }
    ],
  };

  const graficoQuantidadeBud = {
    labels: labels,
    datasets: [
      {
        label: "Quantidade Bud",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: quantidade_bud,
      }
    ],
  };

  const graficoQuantidadeStella = {
    labels: labels,
    datasets: [
      {
        label: "Quantidade Stella",
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(255, 99, 132)",
        data: quantidade_stella,
      }
    ],
  }

  /* NORMAL APP DATA */
  const [modelo, setModelo] = useState("naive");
  const [opt, setOpt] = useState("max");
  const [algoritmo, setAlgoritmo] = useState("mc");
  /* CONTINUE REQUEST */
  const continueReq = (_routeName, _data_split, _iteracoes) => {
    return new Promise((resolve, error) => {
      const procura = arrayToString(_data_split.drink_input);
      const seraFds = arrayToString(_data_split.week_end);
      //MAKE THE FETCH
      return fetch("http://localhost:8000/" + _routeName + "?vetorProcura=" + procura + "&vetorSeraFds=" + seraFds + "&lower=0&upper=100&numero_iteracoes=" + _iteracoes)
        .then((result) => result.json())
        .then(data_algoritmo => {
          const result_v = arrayToString(data_algoritmo.sol);
          //MAKE THE FETCH
          return fetch("http://localhost:8000/inferir?vetor=" + result_v + "&procura=" + procura)
            .then((result) => result.json())
            .then(data_inf => {
              /* SET PROCURA */
              const [budArray, stellaArray] = splitArrayAlternating(_data_split.drink_input);
              setProcuraDataBud(budArray);
              setProcuraDataStella(stellaArray);
              /* SET RETALHISTA */
              setRetalhista(data_inf.list_retalhista);
              /* SET V1 */
              setV1(data_inf.list_v1);
              /* SET V2 */
              setV2(data_inf.list_v2);
              /* SET V3 */
              setV3(data_inf.list_v3);
              /* SET SOBRAS */
              setSobrasBud(data_inf.list_sobras_bud);
              setSobrasStella(data_inf.list_sobras_stella);
              /* SET QUANTIDADES */
              setQtyBud(data_inf.list_quantidade_bud);
              setQtyStella(data_inf.list_quantidade_stella);
              /* SET QUANTIDADES PER V's */
              const [budsv1, budsv2, budsv3, stellasv1, stellasv2, stellasv3] = splitArrayByPattern(data_algoritmo.sol);
              setQtyBudV1(budsv1);
              setQtyBudV2(budsv2);
              setQtyBudV3(budsv3);
              setQtyStellaV1(stellasv1);
              setQtyStellaV2(stellasv2);
              setQtyStellaV3(stellasv3);
              /* SET THE EVALUATION */
              setEvaluation(data_algoritmo.eval[0]);
              const graphsDiv=document.getElementById("graphs-display");
              graphsDiv.classList.remove("display-none");
            })
            .catch((err) => {
              console.log(err);
            })
        })
        .catch((err) => {
          console.log(err);
        })
    })
  }
  /* CHECK IF IT IS NUMERIC */
  const isNumeric = (value) => {
    return !isNaN(parseFloat(value)) && isFinite(value);
  }
  /* FUNCTION TO CHANGE THE MODEL ON CHANGE */
  const onChangeModelo = (event) => {
    setModelo(event.target.value);
  }
  /* FUNCTION TO CHANGE ALGORITHM */
  const onChangeAlgo = (event) => {
    setAlgoritmo(event.target.value);
  }
  /* FUNCTION TO SET THE OPT ON CHANGE */
  const onChangeSetOpt = (event) => {
    setOpt(event.target.value);
  }
  /* FUNCTION TO PUT THE ARRAY IN THE CORRECT STRING FORMAT FOR REQUEST */
  const arrayToString = (array) => {
    return array.join(",");
  }
  /* FUNCTION TO START THE ALGORITHM */
  const makePrevInvi = () => {
    /* GET WEEKS */
    const week_input = document.getElementById("input-semana");
    const week = week_input.value;
    /* GET ITERACOES */
    const iteracoes_input = document.getElementById("input-iteracoes");
    const iteracoes = iteracoes_input.value;
    if (!(week != null && isNumeric(week) && week <= 20 && week >= 1)) {
      alert("Por favor insira um valor nas semanas entre 1 e 20");
      return;
    }
    if (!(iteracoes != null && isNumeric(iteracoes) && iteracoes <= 5000 && iteracoes >= 0)) {
      alert("Por favor insira um valor de iteracoes entre 0 e 5000");
      return;
    }
    if (opt === "max" && algoritmo === "grid") {
      alert("Por favor selecione outras combinacoes de algoritmo e optimizacao");
      return;
    }
    if (opt === "max" && algoritmo === "sann") {
      alert("Por favor selecione outras combinacoes de algoritmo e optimizacao");
      return;
    }
    /* MAKE PREVI INVISIBLE */
    const element = document.getElementById("previ");
    element.classList.add("display-none");
    console.log(modelo);
    //GRAB THE DEMAND FROM THE SPLIT API
    fetch("http://localhost:8000/split?week=" + week + "&bud_model=" + modelo + "&stella_model=" + modelo)
      .then((result_split) => result_split.json())
      .then(data_split => {
        //GRAB THE METHOD
        if (opt === "max") {
          switch (algoritmo) {
            case "mc":
              continueReq("opt-mc", data_split, iteracoes)
                .then((s) => {
                  return;
                })
            case "hc":
              continueReq("opt-hill", data_split, iteracoes)
                .then((s) => {
                  return;
                })
              return;
            default:
              continueReq("opt-hill", data_split, iteracoes)
                .then((s) => {
                  return;
                })
              return;
          }
        } else {
          switch (algoritmo) {
            case "mc":
              continueReq("opt-mc-min", data_split, iteracoes)
                .then((s) => {
                  return;
                })
            case "grid":
              continueReq("opt-grid-min", data_split, iteracoes)
                .then((s) => {
                  return;
                })
            case "hc":
              continueReq("opt-hill-min", data_split, iteracoes)
                .then((s) => {
                  return;
                })
            case "sann":
              continueReq("opt-sann-min", data_split, iteracoes)
                .then((s) => {
                  return;
                })
            default:
              continueReq("opt-grid-min", data_split, iteracoes)
                .then((s) => {
                  return;
                })
          }
        }
      })
      .catch((err) => {
        console.log(err);
      })
  }

  return (
    <div className='simple-column max-width justify-start max-heigth align-center'>
      {/* OPTS AREA */}
      <div id='previ' className="simple-column max-width justify-start max-height align-center">
        <h1>Previsao</h1>
        <div className="simple-row max-width justify-between max-heigth align-center">
          <div className='simple-column max-width justify-start max-heigth align-center'>
            <p>Quantas Semanas?</p>
            <input id='input-semana'></input>
          </div>
          <div className='simple-column max-width justify-start max-heigth align-center'>
            <p>Qual Modelo?</p>
            <form>
              <select onChange={onChangeModelo} className='combo-box' id="myComboModelo" name="myCombo">
                <option  value="naive">naive</option>
                <option  value="ctree">ctree</option>
                <option  value="cv.glmnet">cv glmnet</option>
                <option  value="rpart">rpart</option>
                <option  value="kknn">kknn</option>
                <option  value="ksvm">ksvm</option>
                <option  value="mlp">mlp</option>
                <option  value="mlpe">mlpe</option>
                <option  value="randomForest">randomForest</option>
                <option  value="xgboost">xgboost</option>
                <option  value="cubist">cubist</option>
                <option  value="lm">lm</option>
                <option  value="mr">mr</option>
                <option  value="mars">mars</option>
                <option  value="pcr">pcr</option>
                <option  value="plsr">plsr</option>
                <option  value="cppls">cppls</option>
                <option  value="rvm">rvm</option>
                <option  value="HW">HW</option>
                <option  value="auto.arima">auto arima</option>
                <option  value="ets">ets</option>
                <option  value="nnetar">nnetar</option>
              </select>
            </form>
          </div>
        </div>
        <h1 className='padding-top-20'>Otimizacao</h1>
        <div className="simple-row max-width justify-between max-heigth align-center">
          <div className='simple-column max-width justify-start max-heigth align-center'>
            <p>Pretende..</p>
            <form className='opt-form'>
              <input onChange={onChangeSetOpt} className='larger-radio' type="radio" id="option1" name="radioGroup" value="min" />
              <label for="option1">minimizar</label>

              <input onChange={onChangeSetOpt} className='larger-radio' type="radio" id="option2" name="radioGroup" value="max" />
              <label for="option2">maximizar</label>
            </form>

          </div>
          <div className='simple-column max-width justify-start max-heigth align-center'>
            <p>Qual Algoritmo?</p>
            <form>
              <select className='combo-box' id="myCombo" name="myCombo">
                {opt === "max" ? <div /> : <option onChange={onChangeAlgo} value="grid">grid</option>}
                <option onChange={onChangeAlgo} value="mc">montecarlo</option>
                <option onChange={onChangeAlgo} value="hc">hill climbing</option>
                {opt === "max" ? <div /> : <option onChange={onChangeAlgo} value="sann">sann</option>}
              </select>
            </form>
          </div>
          <div className='simple-column max-width justify-start max-heigth align-center'>
            <p>Quantas Iteracoes?</p>
            <input id="input-iteracoes"></input>
          </div>
        </div>
        <button className='padding-20 button-font' onClick={makePrevInvi}>Confirmar</button>
      </div>
      <div id='graphs-display' className='mg-60 simple-column max-width max-height justify-between'>
        <div className='simple-row padding-30 justify-center'>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20 justify-center'>
            <Line data={graficoProcura} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20 justify-center'>
            <Line data={graficoQBudv1} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20 justify-center'>
            <Line data={graficoQBudv2} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20 justify-center'>
            <Line data={graficoQBudv3} />
          </div>
        </div>
        <div className='simple-row padding-30 justify-center'>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            <Line data={graficoQStellav1} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            <Line data={graficoQStellav2} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            <Line data={graficoQStellav3} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            <Line data={graficoQuantidadeBud} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            <Line data={graficoQuantidadeStella} />
          </div>
        </div>
        <div className='simple-row padding-30 justify-center'>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            <Line data={graficoRetalhista} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            <Line data={graficoSobrasBud} />
          </div>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            <Line data={graficoSobrasStella} />
          </div>
        </div>
        <div className='simple-row padding-30 justify-center'>
          {/* THE RESULT AREA */}
          <div className='bg-white mg-20'>
            Evaluation: {evaluation}
          </div>
        </div>
      </div>
    </div>
  );
}

export default App;
