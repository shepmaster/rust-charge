# syntax=docker/dockerfile:1.5
FROM rust:1.70-bookworm as toolchain

WORKDIR /rust-charge

# ----------
FROM toolchain AS js-builder

RUN \
    --mount=type=cache,target=/var/cache/apt,sharing=locked \
    apt-get update && \
    apt-get -y install nodejs npm && \
    npm install --global pnpm

ENV PNPM_HOME=/var/cache/pnpm

COPY --link . .

RUN \
    --mount=type=cache,target=/var/cache/pnpm \
    --mount=type=cache,target=/rust-charge/node_modules \
    --mount=type=cache,target=/rust-charge/.parcel-cache \
    --mount=type=cache,target=/rust-charge/ui/dist \
    pnpm install --frozen-lockfile && \
    pnpm build && \
    cp -r ui/dist ui/dist-saved

# ----------
FROM toolchain AS rust-builder

RUN \
    --mount=type=cache,target=/var/cache/apt,sharing=locked \
    apt-get update && \
    apt-get -y install libpq-dev

COPY --link . .
COPY --link --from=js-builder /rust-charge/ui/dist-saved/ui.html ./ui/dist/ui.html

RUN \
    --mount=type=cache,target=/usr/local/cargo/registry/ \
    --mount=type=cache,target=/rust-charge/target/ \
    cargo build --release --bin rust-charge && \
    cp target/release/rust-charge ./

# ----------
FROM debian:bookworm AS runtime

WORKDIR /rust-charge

RUN \
    --mount=type=cache,target=/var/cache/apt,sharing=locked \
    apt-get update && \
    apt-get -y install libpq5

COPY --link --from=js-builder /rust-charge/ui/dist-saved/ ./ui/dist/
COPY --link --from=rust-builder /rust-charge/rust-charge .

ENTRYPOINT ["/rust-charge/rust-charge"]
