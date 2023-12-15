# Index Diachronica Redux

Not much documentation here yet, sorry. Check back later.

But here’s what I’ve said [elsewhere](https://verduria.org/viewtopic.php?p=76245#p76245) so far:

> The idea here is that the beginning of each line determines its role:
> `#` to start a series of sound changes between two languages, `@` for sources, `-` for sound changes (think of bullet points), `+` for notes, and `/` for natural-language conditioning factors.
> (`/&` when it entirely replaces the condition in the sound change, `/+` when it merely adds to it.
> Even if it’s replaced by a natural-language conditioning factor, I’m trying to include as many environments as possible in sound-change format, to eventually support searching.)
>
> The sound changes themselves I’m writing in [Brassica format](https://github.com/bradrn/brassica/blob/fdcd7ac63e8cee5377c6af06c934147c6544416d/Documentation.md):
> because it’s capable of representing most of what we need, the syntax should be reasonably familiar, and I’ve written a reusable parser for it already.
> The sources are currently listed in a BibTeX file, with some extra metadata (transcription conventions etc.) in a YAML file.
> For the languages and purported subgroupings, I’m defining unique identifiers for each, listed in a CSV file (similarly to how other projects work such as Glottolog and WALS).

And, one thing I forgot to mention: languages can be associated with (often reconstructed) phonemic inventories, which are listed in another YAML file.

For citations, this currently uses the `chicago-author-date` style from the [CSL project](https://citationstyles.org/).

## Adding new sound changes

The first thing to do is to get your own copy of this repository.
Do so by [forking](https://bradrn/index-diachronica-redux/fork) it on GitHub.
This should give you a web interface where you can add and modify files.
(If you know how to use [`git`](https://www.git-scm.com/) you can clone the repository to work on it locally,
  but then again if you know how to use `git` then you don’t need me to tell you that!)

Now, modify the following files:
- Add a [BibTeX](https://www.bibtex.org/) description of your reference(s) in `data/references.bib`
- Add further metadata about your references in `data/references-data.yaml` (transcriber, confidence ratings and any transcription conventions where it differs from IPA)
- Add a code for each language and subgroup in `data/languoids.csv`
- If available, add information about phoneme inventories and suprasegmentals in `data/langinfo.yaml`
- Now, you can add your sound changes in `data/changes`!
  Make a new file or add to an existing file as appropriate.

Once you’ve done that, [create a pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request)
  from your fork so that your changes are merged into the upstream repository.
