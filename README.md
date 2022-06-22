# Diagnóstico da Segurança Viária nos Municípios Brasileiros

## Introdução 
Esse projeto tem como objetivo auxiliar na criação de um diagnóstico da segurança viária nos municípios brasileiros, levando em consideração a caracterização dos
municípios em relação a aspectos que os diferenciam, como porte, economia, municipalização, entre outros. Com base em 13 indicadores, se buscou estabeler grupos de desempenho através de métodos de redução da dimensionalidade, como a análise de componentes principais (PCA) e estabelecimento de clusters através de k-means. Esse estudo foi realizado pelo [Observatório Nacional de Segurança Viária](https://www.onsv.org.br/). 

## Estrutura
| Arquivo/pasta | Descrição                                     |
| ------------- | --------------------------------------------- |
| `main.R`      | Script principal do projeto                   |
| `dados.R`     | Cria a base de indicadores para os municípios |
| `mapas.R`     | Mapeia todos os indicadores municipais        |
| `calc.R`      | Executa as análises (PCA e k-means)           |
| `tables.R`    | Cria as tabelas com os resultados             |
| `input/`      | Dados de entrada                              |
| `output/`     | Resultados                                    |

## Indicadores
Estabeleceu-se 3 grupos de indicadores: indicadores de resultado final, indicadores de resultado intermediário e indicadores sócioeconômicos

| Indicador | Sigla | Período | Fonte |
| --------- | ----- | ------- | ----- |
| Taxa média de mortes por 100 mil hab. | A1 | 2018 a 2020 | DATASUS |
| Taxa média de mortes por 10 mil veic. | A2 | 2018 a 2020 | DATASUS; DENATRAN |
| Proporção média de mortes de pedestres | A3 | 2018 a 2020 | DATASUS |
| Proporção média de mortes de ciclistas | A4 | 2018 a 2020 | DATASUS |
| Proporção média de mortes de motociclistas | A5 | 2018 a 2020 | DATASUS |
| Idade média da frota | B1 | 2020 | DENATRAN |
| Proporção de motocicletas na frota | B2 | 2020 | DENATRAN |
| Taxa de leitos de internação por 100 mil hab. | B3 | 2020 | DATASUS |
| Taxa de profissionais da saúde por 100 mil hab. | B4 | 2020 | DATASUS |
| Veículos por mil hab. | C1 | 2020 | DENATRAN; DATASUS |
| PIB per capita | C2 | 2019 | IBGE; DATASUS |
| IDH-M | C3 | 2010 | ATLAS-BR |


## Resultados
A análise foi dividida de acordo com o porte dos municípios: menor porte, médio porte ou maior porte.

Os gráficos à seguir apresentam os resultados do PCA e clusterização por k-means dos municípios de médio porte, relacionado aos indicadores de resultado final:

<img src="output/pca_a_Médio porte.png" width=50%> <img src="output/clusters_a_Médio porte.png" width=50%>
