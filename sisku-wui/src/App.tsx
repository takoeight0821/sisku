import './App.css';
import React, { useEffect } from 'react';
import ReactMarkdown from 'react-markdown';
import { Controller, useForm } from 'react-hook-form';
import Container from '@mui/material/Container';
import Stack from '@mui/material/Stack';
import Input from '@mui/material/Input';
import Paper from '@mui/material/Paper';
import Typography from '@mui/material/Typography';
import { Element, Hover, HoverResult } from './lsif';

type SearchResult = {
  hover: HoverResult;
  definition: Element;
  defRanges: Element[];
  moniker: Element;
};

// Get markdown string from Hover.
const getMarkdownString = (hover: Hover): string => {
  if (typeof hover.contents === "string") {
    return hover.contents;
  }
  if (Array.isArray(hover.contents)) {
    return hover.contents.join("\n");
  }
  if ('language' in hover.contents) {
    return '```' + hover.contents.language + '\n' + hover.contents.value + '\n```';
  }
  return hover.contents.value;
}

function App() {
  const endpoint = "http://localhost:8080/search"
  const [query, setQuery] = React.useState('');
  const [searchResults, setSearchResults] = React.useState([] as SearchResult[]);

  const { handleSubmit, control } = useForm();
  const onSubmit = (data: { query: string }) => {
    console.log(data);
    setQuery(data.query);
  };

  useEffect(() => {
    if (query.length > 0) {
      fetch(endpoint + "?q=" + query, { mode: 'cors' })
        .then(response => response.json())
        .then(data => { console.log(data); return data; })
        .then(data => setSearchResults(data))
        .catch(error => {
          console.error(error)
          setSearchResults([])
        })
    }
  }, [query]);

  return (
    <Container sx={{ paddingTop: 2 }}>
      <form onSubmit={handleSubmit(onSubmit)}>
        <Controller
          control={control}
          name="query"
          defaultValue=""
          render={({ field }) => <Input {...field} />}
        />
        <input type="submit" />
      </form>
      <Typography>{query.length > 0 ? `Searching for ${query}` : 'Please enter a search term'}</Typography>
      <Stack spacing={2}>
        {searchResults.map((searchResult, index) =>
          <Paper variant="outlined">
            <Typography variant="subtitle1" sx={{ color: 'primary.main' }}>Result {index}</Typography>
            <Typography variant="subtitle2" sx={{ color: 'secondary.main' }}>Hovers</Typography>
            <ReactMarkdown className="markdown" children={getMarkdownString(searchResult.hover.result)} />
            <Typography variant="subtitle2" sx={{ color: 'secondary.main' }}>Definition</Typography>
            <Typography>{JSON.stringify(searchResult.definition, null, 2)}</Typography>
            <ul>
              {searchResult.defRanges.map((defRange, i) =>
                <li key={i}>
                  <Typography>{JSON.stringify(defRange, null, 2)}</Typography>
                </li>
              )}
            </ul>
            <Typography variant="subtitle2" sx={{ color: 'secondary.main' }}>Moniker</Typography>
            <Typography>{JSON.stringify(searchResult.moniker, null, 2)}</Typography>
          </Paper>
          // <Entry key={i} index={i} searchResult={searchResult} />
        )}
      </Stack>
    </Container>
  );
}

export default App;
