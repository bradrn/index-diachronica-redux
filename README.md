# Index Diachronica Redux

Not much documentation here yet, sorry. Check back later.

But here’s what I’ve said [elsewhere](https://verduria.org/viewtopic.php?p=76245#p76245) so far:

> The idea here is that the beginning of each line determines its role:
> `#` to start a series of sound changes between two languages, `@` for sources, `-` for sound changes (think of bullet points), `+` for notes, and `/` for natural-language conditioning factors.
> (`/&` when it entirely replaces the condition in the sound change, `/+` when it merely adds to it.
> Even if it’s replaced by a natural-language conditioning factor, I’m trying to include as many environments as possible in sound-change format, to eventually support searching.)
>
> The sound changes themselves I’m writing in [Brassica format](https://github.com/bradrn/brassica/blob/master/Documentation.md):
> because it’s capable of representing most of what we need, the syntax should be reasonably familiar, and I’ve written a reusable parser for it already.
> The sources are currently listed in a BibTeX file, with some extra metadata (transcription conventions etc.) in a YAML file.
> For the languages and purported subgroupings, I’m defining unique identifiers for each, listed in a CSV file (similarly to how other projects work such as Glottolog and WALS).

And, one thing I forgot to mention: languages can be associated with (often reconstructed) phonemic inventories, which are listed in another YAML file.

For citations, this currently uses the `chicago-author-date` style from the [CSL project](https://citationstyles.org/).
