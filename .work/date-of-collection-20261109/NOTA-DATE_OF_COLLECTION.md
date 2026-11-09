# DATE_OF_COLLECTION y DATE_OF_PRESERVATION en Collection_data

Nota para el equipo (9 nov 2026). Todo esto se leyó de la copia de la app del 9 nov. No se editó nada en el Sheet.

## Qué calcula la fórmula

Todas las filas de Collection_data (9754) tienen esta fórmula en DATE_OF_COLLECTION (columna BO):

```
=IFS(AD2="","",AD2="NA","NA",
     AND(AD2<DATE(2023,4,14), XLOOKUP($R2,Lists!$AG$1:$AG$10,Lists!$AH$1:$AH$10,"NO")="NO"), AD2+451,
     TRUE, AD2)
```

AD es Collection_date y R es Genus. Lists!AG:AH es la lista «Genera_0176», con nueve géneros marcados YES:
Heliconius, Mechanitis, Melinaea, Hypothyris, Napeogenes, Hyposcada, Oleria, Brevioleria y Chetone.

Esto significa que cuando una fila tiene fecha anterior al 14-abr-2023 y su género no está en esa lista,
DATE_OF_COLLECTION muestra **Collection_date + 451 días**. En las demás filas copia Collection_date.
DATE_OF_PRESERVATION (columna BX) hace lo mismo con Preservation_date (AL), salvo que para un NA da
«NOT_COLLECTED».

Un ejemplo: la colecta más antigua que se corre es del 18-ene-2022, y 18-ene-2022 + 451 días = 14-abr-2023.
Por eso las fechas corridas van del 14-abr-2023 (colectas del 18-ene-2022) al 07-jul-2024 (colectas del
13-abr-2023), y ninguna queda antes del 14-abr-2023.

## Cuántas filas cambian

| | Filas con fecha distinta | De 2022 | De 2023 (antes del 14-abr) |
|---|---|---|---|
| DATE_OF_COLLECTION frente a Collection_date | **759** | 697 | 62 |
| DATE_OF_PRESERVATION frente a Preservation_date | **755** | 694 | 61 |

En todas la diferencia es exactamente 451 días. Los géneros con más filas corridas son Ithomia (146),
Godyris (103), Ceratinia (71), Pteronymia (54), Pseudoscada (51) y Forbestra (51). Las localidades con más
filas son Wildsumaco (77), Puerto Libre - Linea 2 (71), Apuya 2.7km (68), Río Pusuno (65), Yanayacu (44)
y San Pedro de Arajuno (43).

Un caso concreto: Río Pusuno se colectó el 21-ene-2022 y el 18-jul-2022 según Collection_date.
DATE_OF_COLLECTION añade dos fechas que no existen, el 17-abr-2023 y el 12-oct-2023. Las dos Tithorea
harmonia (CAM070542, CAM070543) son del 18-jul-2022, pero DATE_OF_COLLECTION dice 12-oct-2023.

## Otras hojas

- **Insectary_data**: DATE_OF_COLLECTION es `=M2`, o sea **Preservation_date** (20834 filas), no una fecha
  de colecta.
- **F1/F2_MutationRate**: DATE_OF_COLLECTION busca la fecha en Collection_data!AD (si es silvestre) o en
  Insectary_data!M, que es la preservación (si es criada). No tiene el corrimiento.
- **Ithomiini_tube_locations_18Jun26**: busca en la hoja «Genus&CollectionDate_…», que ya no existe, y por
  eso da #REF!.

## Qué lo usa

- Las pantallas de la app y los mapas usan Collection_date. Solo los reportes de filas por día o por semana
  de la app usan DATE_OF_COLLECTION, y solo como respaldo cuando una fila no tiene Intro2Insectary_date ni
  Collection_date. En Insectary_data eso cuenta esas filas en su fecha de preservación.
- Desde la versión de la app del 9 nov, cuando el asistente consulta DATE_OF_COLLECTION o
  DATE_OF_PRESERVATION, sus herramientas le avisan que son fórmulas del manifiesto y que la fecha de colecta
  está en Collection_date.
- Las hojas MEIER_manifests del libro no tienen una columna de fecha de colecta. Por eso desde aquí no se
  puede saber si a Sanger se mandaron las fechas corridas. Eso lo sabe PAS.

## Fórmula propuesta

Si DATE_OF_COLLECTION debe ser la fecha de colecta, esta es la fórmula de la fila 2 de Collection_data.
Mantiene las salidas de la fórmula actual para celdas vacías y NA:

```
DATE_OF_COLLECTION (BO):   =IFS(AD2="","",AD2="NA","NA",TRUE,AD2)
DATE_OF_PRESERVATION (BX): =IFS(AL2="","",AL2="NA","NOT_COLLECTED",TRUE,AL2)
```

Si el corrimiento se hizo a propósito (por ejemplo, por la fecha de un permiso), conviene que PAS lo explique
en Notes o en las instrucciones de la hoja. Así nadie toma esas fechas como fechas de colecta.
