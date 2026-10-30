# Data benchmarks for the borrowing limit and the informal premium

Used by notebook 8, Step 11b (and read again by notebook 8b, Step 6) to check whether the
values of `a_min` and `theta_I` each model needs are realistic.

| File | Content | Source |
|---|---|---|
| `bis_household_credit_argentina.csv` | Credit to households and NPISHs, all sectors, % of GDP, quarterly 1994–2026, adjusted for breaks (series `Q.AR.H.A.M.770.A`) | BIS total credit statistics, downloaded 2026-10-30 from `https://stats.bis.org/api/v2/data/dataflow/BIS/WS_TC/2.0/Q.AR.H.A.M.770.A?format=csv` |
| `wb_household_consumption_argentina.csv` | Household and NPISH final consumption, % of GDP, annual 1990–2025 (`NE.CON.PRVT.ZS`) | World Bank WDI API, downloaded 2026-10-30 |
| `bcra_personal_loan_rates.csv` | Nominal annual rates (TNA) on personal loans: banks vs non-bank credit providers (OPNFC) | Transcribed from the BCRA *Informe de Proveedores No Financieros de Crédito* (Dec 2023, Nov 2025, Jun 2026); page numbers in the file |
| `bcra_household_credit_facts.csv` | Size of non-bank relative to bank credit to individuals; delinquency; inflation | Same BCRA reports |

Report PDFs: `https://www.bcra.gob.ar/informe-de-proveedores-no-financieros-de-credito/`.
