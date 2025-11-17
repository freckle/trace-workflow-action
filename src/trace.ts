import {
  type Tracer,
  TimeInput,
  trace,
  context,
  SpanStatusCode,
  Span,
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
  parentSpan?: Span
): Span | undefined {
  const { name, started_at, completed_at, conclusion } = traceable;

  if (!name) {
    console.debug("span has no name, skipping");
    return;
  }

  if (!completed_at) {
    console.debug("span has no completed_at, skipping");
    return;
  }

  // don't create spans for skipped steps
  if (conclusion === 'skipped') {
    return;
  }

  // omit spans for lightweight steps which clutter the flame graph
  if (name.startsWith("Post Run" ) || name === 'Complete job') {
    return;
  }

  let ctx;
  if (parentSpan != null) {
    console.debug(`setting span ${parentSpan.spanContext().spanId} as parent for ${name}`);
    ctx = trace.setSpan(
      context.active(),
      parentSpan,
    );
  }

  // console.debug(`Begin span: ${name}: ${started_at}`);
  const span = tracer.startSpan(
    name,
    { startTime: toTimeInput(started_at) },
    ctx
  );

  for (const tag of tags || []) {
    span.setAttribute(tag.key, tag.value);
  }

  if (conclusion === "failure") {
    span.setStatus({
      code: SpanStatusCode.ERROR,
      message: "Operation failed",
    });
  }
  else {
    span.setStatus({
      code: SpanStatusCode.OK,
      message: "Operation succeeded",
    });
  }
  span.end(toTimeInput(completed_at));

  return span;
}

function toTimeInput(t: string | null | undefined): TimeInput {
  if (!t) {
    throw new Error("TODO");
  }

  return new Date(t);
}
