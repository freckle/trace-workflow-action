import {
  type Tracer,
  TimeInput,
  trace,
  context,
  SpanStatusCode,
} from "@opentelemetry/api";

import { BasicTracerProvider } from "@opentelemetry/sdk-trace-base";
import { NodeSDK } from "@opentelemetry/sdk-node";
import {
  SEMRESATTRS_SERVICE_NAME,
  SEMRESATTRS_SERVICE_VERSION,
} from "@opentelemetry/semantic-conventions";
import { Resource } from "@opentelemetry/resources";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";

const sdk = new NodeSDK({
  resource: new Resource({
    [SEMRESATTRS_SERVICE_NAME]: "github-actions",
    [SEMRESATTRS_SERVICE_VERSION]: "1.0.0"
  }),
  traceExporter: new OTLPTraceExporter({
    url: "http://localhost:4318",
  }),
});

sdk.start();

// const exporterOptions = {
//   serviceName: "my-service-name",
//   url: "http://localhost:4318",
//  }

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
