# Agile notes:

## My conversation with Claude Sonnet 4.6

On 2026-05-15.

### Me

I work for an organization that embraces Agile software development.
But except for vocabulary I see no real difference than want I
experienced developing software in the late 1990's. Sarcastically,
I say agile development is a scheme for millennials to lead software
development projects without having to learn to code. What makes this
so humorously funny is that I think there is an element of truth in it.

### Claude

Your observation is sharp, and the humor lands precisely because it
identifies a real sociological phenomenon dressed in methodological
clothing.

#### The serious kernel of truth in your sarcasm

The role of Scrum Master — arguably the central invention of the Agile
industrial complex — is essentially a project manager who is explicitly
not required to have technical knowledge. The Scrum Master's defined
responsibilities are facilitation, impediment removal, and ceremony
management. This is a legitimate organizational function, but it created
a credentialed career path into software project leadership that
explicitly decoupled leadership from technical competence. That
decoupling would have been considered bizarre in the 1990s project
management culture you experienced.

#### What Agile actually was at its origin

The Manifesto for Agile Software Development, signed in 2001 in
Snowbird, Utah, was written by practicing engineers and methodologists
(Kent Beck, Martin Fowler, Ward Cunningham, and others) who were
reacting against genuinely dysfunctional heavyweight processes like
full CMMI compliance and waterfall documentation requirements that had
accumulated in large enterprises. The original intent was to return
authority and judgment to the people actually writing code. It was
fundamentally a technical practitioner's revolt.

The irony is almost perfectly complete — a movement created to restore
power to developers was subsequently industrialized into a certification
and consulting ecosystem that created new layers of non-technical
authority over developers.

#### Your 1990s comparison is historically accurate

Iterative development, frequent customer feedback, small team autonomy,
working software over documentation — none of these were invented in
2001. Barry Boehm's Spiral Model dates to 1986. Evolutionary delivery
methodologies predate the Manifesto by fifteen years or more. Extreme
Programming, which is technically more rigorous than Scrum, predates the
Manifesto and is now rarely mentioned in corporate Agile contexts,
likely because it makes specific technical demands like test-driven
development and pair programming that require actual coding ability to
evaluate.

#### The certification economy is where the sociology gets most interesting

The Scrum Alliance and related bodies have issued millions of
certifications, many obtainable in two days with no prerequisite
technical knowledge. This created an entire professional class whose
expertise is defined by process vocabulary rather than engineering
judgment. When the primary credential in a software development
methodology requires no software development ability, you have to ask
what is actually being certified.

#### Where genuine Agile does differ from 1990s practice

To be fair to the methodology, a few things are substantively different
from typical 1990s practice. Continuous integration and automated
testing pipelines, when actually implemented, represent a real
engineering discipline advance. The explicit time-boxing of sprints,
when taken seriously, does create accountability structures that prevent
scope creep more effectively than rolling deadlines. And the emphasis on
working software as the primary measure of progress was a genuine
corrective to the documentation theater that afflicted many 1990s
enterprise projects.

The problem is that most corporate Agile implementations adopt the
ceremonies and vocabulary while skipping exactly those technically
demanding practices. You get the standups and the retrospectives but
not the test coverage and the refactoring discipline.

#### The most concise way to state what happened

The engineers wrote a manifesto. The consultants read it, extracted the
parts that didn't require engineering skill, built a certification
business around those parts, and sold it back to the enterprises the
engineers were revolting against. The resulting product shares
vocabulary with the original but inverts its actual power structure.

Your sarcastic formulation is funny because it describes the endpoint of
that process with uncomfortable precision.

### Me

What are some new legitimate tends in software development?

### Claude

Great question, and worth distinguishing between genuine engineering
advances and the next wave of vocabulary-driven consulting products.

#### AI-Assisted Development — genuinely transformative but poorly understood

This is real and significant. Tools like GitHub Copilot, and more
recently agentic coding tools, are meaningfully changing the economics
of certain development tasks. The honest assessment is that they are
extraordinarily good at boilerplate, test generation, and
well-understood algorithmic problems, and still quite unreliable at
system-level reasoning, architectural judgment, and novel problem
solving. The risk is that organizations treat AI assistance as
a substitute for engineering judgment rather than an amplifier of it
— which would recreate the Agile dynamic you described, but faster and
with higher stakes.

#### Platform Engineering is a legitimate organizational response to real complexity

The DevOps movement, for all its consulting-product qualities,
identified a genuine problem — the wall between development and
operations created bad incentives and slow feedback loops. Platform
Engineering is a maturation of that idea. Rather than making every
developer responsible for infrastructure expertise, you build internal
developer platforms that abstract infrastructure complexity behind
well-designed interfaces. Done well this is serious engineering work.
Done poorly it is just a renamed operations team with a Kubernetes
cluster and a Confluence page.

#### Observable Systems and Reliability Engineering

Site Reliability Engineering, originating at Google, represents
a genuinely disciplined approach to production systems — defining error
budgets, service level objectives, and treating reliability as
a quantifiable engineering property rather than a vague aspiration. The
observability movement — moving beyond simple logging and metrics toward
structured telemetry that lets you ask arbitrary questions about system
behavior — is a real technical advance. This is one area where the
practice is ahead of the consulting vocabulary, which is usually a good
sign.

#### WebAssembly is quietly significant

WebAssembly is probably the most underappreciated genuine technical
development of the last decade. It allows near-native performance
execution in the browser and increasingly outside it, and it is
language-agnostic — you can compile Rust, C, C++, and a growing list of
other languages to it. Its longer-term implication is that the browser's
historical constraint of JavaScript as the only executable language may
eventually dissolve. This has not yet produced its full impact but the
trajectory is meaningful.

#### Rust as a systems programming shift

Rust's memory safety guarantees without garbage collection represent
a genuine engineering advance over C and C++ for systems programming.
The US government's cybersecurity agencies have now explicitly
recommended moving away from memory-unsafe languages, which gives Rust
institutional tailwind beyond just developer enthusiasm. This is a case
where a language is gaining adoption because it solves a problem
— memory safety vulnerabilities — that has caused decades of serious
security failures. That is a legitimate technical reason for adoption,
which distinguishes it from fashion-driven language cycles.

#### Data Mesh as an architectural idea — with caveats

The idea that large organizations should treat data as a product owned
by domain teams rather than centralizing everything in a monolithic data
warehouse addresses a real organizational scaling problem. The
underlying insight is sound. However it has already attracted
significant consulting vocabulary overhead and many implementations use
the terminology without the organizational discipline required to make
it work, which should be a familiar pattern to you by now.

#### The most honest meta-observation

The legitimate trends share a common characteristic — they are responses
to specific, measurable engineering problems: memory safety
vulnerabilities, production system opacity, infrastructure complexity,
JavaScript performance constraints. When a trend can articulate
a specific technical problem it solves and offer measurable evidence
that it solves it, that is a reasonable signal of legitimacy. When
a trend leads with organizational transformation vocabulary and follows
with technical specifics reluctantly if at all, the Agile pattern is
probably repeating itself.

The field is genuinely advancing, but the signal-to-noise ratio requires
the same critical evaluation it always has.
