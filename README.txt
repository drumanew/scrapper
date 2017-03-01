To make:
  $ make all

To run:
  $ make shell

  Eshell
  1> application:ensure_all_started(scrapper).

To show active workers:
  Eshell
  2> scrapper_broker:active_jobs().
  [<0.101.0>,<0.131.0>,<0.163.0>,<0.191.0>] %% example

To ask worker what he is doing:
  Eshell
  2> [ scrapper_worker:get_state(Pid) || Pid <- scrapper_broker:active_jobs() ].
  [{"https://www.amazon.com/Letter-Spirit-Vol-Salvation-Mysteries-ebook/dp/B00342WIBA",
    "178.22.148.122:3129",waiting},
   {"https://www.amazon.com/Getting-Marriage-Conversation-Right-Effective-ebook/dp/B009R4RR88",
    "178.22.148.122:3129",
    {downloading,12017}},
   {"https://www.amazon.com/Navegando-Vida-Interior-Direcci%C3%B3n-Espiritual-ebook/dp/B00N27GGH4",
    "178.22.148.122:3129",waiting}]
