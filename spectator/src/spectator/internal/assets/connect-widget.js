const dialogHtml = `
    <dialog class="connect-widget-dialog">
      <div class="connect-widget-content">
        <h2>Inspect BEAM Node</h2>
         <div class="connect-widget-form">
          <label for="node-name">Node Name</label>
          <input required="true" type="text" id="node-name" name="node-address" placeholder="wibble@127.0.0.1" />
          <label for="refresh-interval">Refresh Interval</label>
          <select id="refresh-interval" name="refresh-interval">
            <option value="">Default</option>
            <option value="500">500 milliseconds</option>
            <option value="1000">1 second</option>
            <option value="2000">2 seconds</option>
            <option value="5000">5 seconds</option>
            <option value="10000">10 seconds</option>
            <option value="30000">30 seconds</option>
            <option value="60000">1 minute</option>
          </select>
         </div>
        <div class="connect-widget-actions">
          <button title="Close this dialog" id="connect-cancel">Cancel</button>
          <button title="Inspect the node that spectator itself is running on" id="connect-resetl">Inspect Spectator Node</button>
          <button title="Connect to specified node" id="connect-submit">Connect</button>
        </div>
      </div>
    </dialog>
  `;

const template = document.createElement("template");
template.innerHTML = dialogHtml.trim();
const dialogElement = template.content.firstChild;
document.body.appendChild(dialogElement);

const cancelButton = dialogElement.querySelector("#connect-cancel");
const resetButton = dialogElement.querySelector("#connect-resetl");
const submitButton = dialogElement.querySelector("#connect-submit");
const nodeNameInput = dialogElement.querySelector("#node-name");
const refreshIntervalSelect = dialogElement.querySelector("#refresh-interval");

// Prefill with current params
const urlParams = new URLSearchParams(window.location.search);
const currentNode = urlParams.get("node") || "";
const currentInterval = urlParams.get("refresh") || "";
nodeNameInput.value = currentNode;
refreshIntervalSelect.value = currentInterval;

submitButton.addEventListener("click", () => {
  const nodeName = nodeNameInput.value;
  const refreshInterval = refreshIntervalSelect.value;

  if (nodeNameInput.reportValidity()) {
    const newParams = new URLSearchParams();
    newParams.set("node", nodeName);
    if (refreshInterval !== "") {
      newParams.set("refresh", refreshInterval);
    }
    window.location.search = newParams.toString();
  }
});

cancelButton.addEventListener("click", () => {
  dialogElement.close();
});

resetButton.addEventListener("click", () => {
  window.location.search = "";
  dialogElement.close();
});

document.querySelectorAll(".change-target-button").forEach((button) => {
  button.style.display = "inline-block";
  button.addEventListener("click", dialogElement.showModal.bind(dialogElement));
});