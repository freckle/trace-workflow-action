import {
  type Tracer,
  TimeInput,
  trace,
  context,
  SpanStatusCode,
} from "@opentelemetry/api";

export type tag = {
  key: string,
  value: string
}

export function getTracer(): Tracer {
  return trace.getTracer("freckle-trace-workflow-action");
}

export interface Traceable {
  name: string | null | undefined;
  started_at?: string | null;
  completed_at?: string | null;
  conclusion: string | null;
}

export function inSpan(
  tracer: Tracer,
  traceable: Traceable,
  tags?: tag[],
  fn?: () => void
): void {
  const { name, started_at, completed_at, conclusion } = traceable;

  if (!name) {
    throw new Error("TODO");
  }

  // don't create spans for skipped steps
  if (conclusion === 'skipped') {
    return;
  }

  // omit spans for lightweight steps which clutter the flame graph
  if (name.startsWith("Post Run" ) || name === 'Complete job') {
    return;
  }

  // console.log(`Span: ${name}: ${started_at}`);
  const span = tracer.startActiveSpan(
    name,
    { startTime: toTimeInput(started_at) },
    (span) => {
      for (const tag of tags || []) {
        span.setAttribute(tag.key, tag.value);
      }
      if (fn) {
        fn();
      }

      if (conclusion === "failure") {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: "Operation failed",
        });
      }

      span.end(toTimeInput(completed_at));
    }
  );
}

function toTimeInput(t: string | null | undefined): TimeInput {
  if (!t) {
    throw new Error("TODO");
  }

  return new Date(t);
}
