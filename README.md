[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![Apache 2.0 License][license-shield]][license-url]
[![DOCS][docs-shield]][docs-url]
[![LinkedIn][linkedin-shield]][linkedin-url]

-----
# Match: Erlang-Based Expert System Applications
-----

**Match** is a monorepo showcasing a collection of Erlang applications built around the **SERESYE** (Swarm oriented ERlang Expert SYstem Engine). It demonstrates the capabilities of using a Rete-based rules engine in Erlang for various tasks, including semantic data processing and job matching.

This project is built using [![Rebar3][rebar3-shield]][rebar3-url] on [![Erlang][erlang-shield]][erlang-url].

-----
## About The Project

The `match` repository serves as a central hub for several interconnected Erlang applications:

*   **SERESYE (`apps/seresye`)**: The core of the project, SERESYE is a Rete-based rules engine written in Erlang. It allows users to define a knowledge base (facts) and production rules to infer new knowledge and trigger actions. For detailed information, refer to the [SERESYE README](apps/seresye/README.md).

*   **Semantic Relatives (`apps/semantic_relatives`)**: This application demonstrates the use of SERESYE in conjunction with a Semantic Web Toolkit. It processes semantic web data (ontologies) to understand relationships (e.g., family trees) and stores this information in a Mnesia database. See the [Semantic Relatives README](apps/semantic_relatives/README.md) for more.

*   **Job Match (`apps/job_match`)**: A practical application showcasing SERESYE's capabilities. It decodes job posting and applicant ontologies, builds a knowledge base, and uses a rule engine to match suitable applicants to job openings, evaluating them based on various criteria. Explore the [Job Match README](apps/job_match/README.md) for specifics.

*   **Match (`apps/match`)**: This application appears to be the umbrella application for the release, tying together the other components.

-----
## Getting Started

### Prerequisites

Ensure you have Erlang/OTP installed (developed with OTP 23). You will also need Rebar3 to build the project.

*   **Erlang/OTP**: [Installation Guide](http://www.erlang.org/downloads)
*   **Rebar3**: [Installation Guide](https://rebar3.org/docs/getting-started/)

### Build

To compile the entire project and all its applications:

```bash
rebar3 compile
```

To open an Erlang shell with the project's applications loaded:

```bash
rebar3 shell
```

-----
## Usage

Each application within the `apps/` directory has its own specific usage instructions and examples in its respective `README.md` file.

A primary example to explore is the **Job Match** application:

1.  Build and start the shell:
    ```bash
    rebar3 compile
    rebar3 shell
    ```
2.  From the Erlang shell, start the `job_match` application:
    ```erlang
    1> job_match:start().
    ok
    ```
3.  You can then query the `job_match` engine. For example, to retrieve all facts in its knowledge base:
    ```erlang
    2> seresye_srv:get_kb(job_match).
    [{match,<<"JPosting22">>,<<"JSeeker126">>,... % and many more
    ```
    Refer to the [Job Match README](apps/job_match/README.md) for more detailed query examples.

Similarly, you can explore the `semantic_relatives` application by following the instructions in its [README](apps/semantic_relatives/README.md).

-----
## Diagram

(The original README mentioned a diagram. If a diagram exists or can be generated, it would be beneficial to include it here, perhaps as an image or a link to a generated document.)
```
[//]: # (Consider adding a project architecture diagram if available)
```

-----
## Contributing

Contributions are welcome! Please feel free to fork the repository, make changes, and submit pull requests. If you find any issues or have suggestions for improvements, please open an issue on GitHub.

-----
## License

Distributed under the Apache License 2.0. See `LICENSE` for more information.

-----
## Contact

Miloud Eloumri - [LinkedIn][linkedin-url] - miloud.eloumri@gmail.com

Project Link: [https://github.com/MiloudEloumri/match](https://github.com/MiloudEloumri/match)

-----
## Acknowledgements

*   ERESYE project (inspiration for SERESYE)
*   Semantic Web ToolKit for Erlang
*   Rebar3
*   Erlang/OTP

-----

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/MiloudEloumri/match.svg?style=for-the-badge
[contributors-url]: https://github.com/MiloudEloumri/match/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/MiloudEloumri/match.svg?style=for-the-badge
[forks-url]: https://github.com/MiloudEloumri/match/network/members
[stars-shield]: https://img.shields.io/github/stars/MiloudEloumri/match.svg?style=for-the-badge
[stars-url]: https://github.com/MiloudEloumri/match/stargazers
[issues-shield]: https://img.shields.io/github/issues/MiloudEloumri/match.svg?style=for-the-badge
[issues-url]: https://github.com/MiloudEloumri/match/issues
[license-shield]: https://img.shields.io/badge/License-Apache%202.0-blue.svg?style=for-the-badge
[license-url]: https://github.com/MiloudEloumri/match/blob/main/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/in/miloudeloumri/
[rebar3-shield]: https://img.shields.io/badge/Rebar3-3.17.0-blue?labelColor=A90533&style=plastic
[rebar3-url]: https://github.com/erlang/rebar3
[erlang-shield]: https://img.shields.io/badge/Erlang|OTP_23-Erts_11.0-blue?labelColor=A90533&style=plastic&logo=Erlang
[erlang-url]: http://www.erlang.org
[docs-shield]: https://img.shields.io/static/v1?label=Docs&message=Read&&Color=8CA1AF&logo=Read-the-Docs&style=for-the-badge
[docs-url]: https://github.com/MiloudEloumri/match/tree/main/doc
