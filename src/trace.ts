import {
  type Tracer,
  TimeInput,
  trace,
  context,
  SpanStatusCode,
} from "@opentelemetry/api";
import { Resource } from "@opentelemetry/resources";
import {
  SEMRESATTRS_SERVICE_NAME,
  SEMRESATTRS_SERVICE_VERSION,
} from "@opentelemetry/semantic-conventions";
import { WebTracerProvider } from "@opentelemetry/sdk-trace-web";
import {
  BatchSpanProcessor,
  ConsoleSpanExporter,
} from "@opentelemetry/sdk-trace-base";

const resource = Resource.default().merge(
  new Resource({
    [SEMRESATTRS_SERVICE_NAME]: "todo",
    [SEMRESATTRS_SERVICE_VERSION]: "0.0.0",
  })
);

const provider = new WebTracerProvider({
  resource: resource,
});
const exporter = new ConsoleSpanExporter();
const processor = new BatchSpanProcessor(exporter);
provider.addSpanProcessor(processor);

provider.register();

export function getTracer(): Tracer {
  return trace.getTracer("freckle-trace-workflow-action");
}

export interface Traceable {
  name: string | null | undefined;
  started_at?: string | null;
  completed_at?: string | null;
  status: string | null;
}

export function inSpan(
  tracer: Tracer,
  traceable: Traceable,
  fn?: () => void
): void {
  const { name, started_at, completed_at, status } = traceable;

  if (!name) {
    throw new Error("TODO");
  }

  //console.log(`Span: ${name}: ${started_at}`);
  const ctx = context.active();
  const span = tracer.startSpan(
    name,
    { startTime: toTimeInput(started_at) },
    ctx
  );

  if (fn) {
    fn();
  }

  if (status === "failed") {
    span.setStatus({ code: SpanStatusCode.ERROR, message: "Operation failed" });
  }

  // console.log(` End: ${name}: ${completed_at} (${error})`);
  span.end(toTimeInput(completed_at));
}

function toTimeInput(t: string | null | undefined): TimeInput {
  if (!t) {
    throw new Error("TODO");
  }

  return new Date(t);
}
