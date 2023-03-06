/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Run conformance tests, which are used by the Go starlark.
//! e.g. <https://github.com/google/skylark/tree/master/testdata>

// `if_then_panic` is only in newer clippy, delete this in future.
#![allow(unknown_lints)]
// We want to carefully control the panic message.
#![allow(clippy::if_then_panic)]

use gazebo::prelude::*;
use itertools::Itertools;

/// Describe a conformance test
struct ConformanceTest {
    /// The code of the test
    code: String,
    /// If this might throw an error, what is it
    error: Option<(usize, String)>,
}

impl ConformanceTest {
    fn parse(code: &str) -> Vec<Self> {
        // First split on "---"
        code.lines()
            .collect::<Vec<_>>()
            .split(|x| *x == "---")
            .map(|xs| Self {
                code: xs.join("\n"),
                error: xs
                    .iter()
                    .find_position(|x| x.contains("###"))
                    .map(|(i, x)| (i + 1, (**x).split1("###").1.trim_start().to_owned())),
            })
            .collect()
    }
}
