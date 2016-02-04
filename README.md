# 𝜋-Base 2

An in-progress rewrite of [the 𝜋-Base topology database](https://github.com/jamesdabbs/pi-base.hs).

Major goals are:

* Extract business logic apart from persistence layer (and allow treatment of multiple axiom sets)
* Separate out a useable API and a JS-based frontend client
* Tests!
* Use Servant to generate a JS client and documentation
* Rewrite auth system (since Persona is going away)