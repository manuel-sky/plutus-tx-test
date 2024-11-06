export function findUTXOWithSpecificUnit(utxos, unitToFind) {
    for (const utxo of utxos) {
        for (const amount of utxo.output.amount) {
            if (amount.unit === unitToFind) {
                return utxo;
            }
        }
    }
    return null;
}
