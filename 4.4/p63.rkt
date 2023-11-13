(rule (grandson ?grandfather ?grandson)
      (and
        (son ?grandfather ?father)
        (son ?father ?son)))

(rule (son-of-mother ?mother ?son)
      (and
        (wife ?father ?mother)
        (son ?father ?son)))
