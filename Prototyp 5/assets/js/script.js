import Scatterplot from './deepscatter.js';

// Build scatterplot

const DATA_URL = 'https://textminr.s3.eu-north-1.amazonaws.com/data/scatterplot/gutenberg';
const prefs = {
  source_url: DATA_URL + '/tiles',
  max_points: 50000,
  alpha: 10.12,
  zoom_balance: 0.12,
  point_size: 5,
  background_color: '#172554',
  click_function: null,
  encoding: {
    jitter_radius: {
      constant: 0.001,
      method: 'normal',
    },
    color: {
      field: 'topic',
      range: 'category10'
    },
    x: {
      field: 'x',
      transform: 'literal',
    },
    y: {
      field: 'y',
      transform: 'literal',
    },
    filter: {
      field: 'topic',
      lambda: 'x => x !== "Unclustered"'
    },
  },
  labels: {
    url: DATA_URL + '/topics.geojson',
    name: undefined,
    label_field: 'topic',
    size_field: undefined,
  },
};

const scatterplot = new Scatterplot('#scatterplot');
const generateTooltipHTML = (text) => {
  const filteredEntries = Object.entries(text).filter(([key]) => !['x', 'y', 'ix'].includes(key));
  const html = filteredEntries.map(([key, value]) => `<span class="font-bold">${key}:</span> ${value}`).join('<br>');
  return `<p class="text-black">${html}</p>`;
};
scatterplot.tooltip_html = generateTooltipHTML;
scatterplot.plotAPI(prefs);

// Handle shiny events

const applyYearFilter = (years) => {
  scatterplot.plotAPI({
    encoding: {
        filter: {
          field: 'topic',
          lambda: 'x => x !== "Unclustered"'
        },
        filter2: {
            op: 'between',
            field: 'year',
            a: years[0],
            b: years[1]
        }
    }
  })
}
Shiny.addCustomMessageHandler('range', applyYearFilter);