import React, { useEffect } from 'react';
import './App.css';
import ReactMarkdown from 'react-markdown';
import Container from '@mui/material/Container';
import TextField from '@mui/material/TextField';
import Typography from '@mui/material/Typography';
import {Element, Hover, HoverResult} from './lsif';

type SearchResult = {
  hover: HoverResult;
  definition: Element;
  defRanges: Element[];
  moniker: Element;
};

const SearchBar = ({ onSearchTermChange }: { onSearchTermChange: (searchTerm: string) => void }) => {
  return <TextField label="Search" onChange={(e) => onSearchTermChange(e.target.value)} />
};

const SearchResults = ({ searchResults }: { searchResults: SearchResult[] }) => {
  return <div>
    {searchResults.map((searchResult, i) =>
      <Entry key={i} index={i} searchResult={searchResult} />
    )}
  </div>
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

const Entry = ({ index, searchResult }: { index: number, searchResult: SearchResult }) => {
  return <>
    <Typography variant="subtitle1">Result {index}</Typography>
    <Typography variant="subtitle2">Hovers</Typography>
    <ReactMarkdown children={getMarkdownString(searchResult.hover.result)} />
    <Typography variant="subtitle2">Definition</Typography>
    <Typography>{JSON.stringify(searchResult.definition)}</Typography>
    <ul>
      {searchResult.defRanges.map((defRange, i) =>
        <li key={i}>
          <Typography>{JSON.stringify(defRange)}</Typography>
        </li>
      )}
    </ul>
    <Typography variant="subtitle2">Moniker</Typography>
    <Typography>{JSON.stringify(searchResult.moniker)}</Typography>
  </>
};

function App() {
  const endpoint = "http://localhost:8080/search"
  const [searchTerm, setSearchTerm] = React.useState('');
  const [searchResults, setSearchResults] = React.useState([] as SearchResult[]);

  useEffect(() => {
    if (searchTerm.length > 0) {
      fetch(endpoint + "?q=" + searchTerm, { mode: 'cors' })
        .then(response => response.json())
        .then(data => { console.log(data); return data; })
        .then(data => setSearchResults(data))
        .catch(error => {
          console.error(error)
          setSearchResults([])
        })
    }
  }, [searchTerm]);

  return (
    <Container sx={{ paddingTop: 2 }}>
      <SearchBar onSearchTermChange={setSearchTerm} />
      <Typography>{searchTerm.length > 0 ? `Searching for ${searchTerm}` : 'Please enter a search term'}</Typography>
      <SearchResults searchResults={searchResults} />
    </Container>
  );
}

export default App;
