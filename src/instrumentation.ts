/*instrumentation.ts*/
import * as api from '@opentelemetry/api';
import { Resource } from '@opentelemetry/resources';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-proto';
import { BasicTracerProvider, ConsoleSpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-node';
import { SEMRESATTRS_SERVICE_NAME } from '@opentelemetry/semantic-conventions';
import { JaegerExporter } from "@opentelemetry/exporter-jaeger";
import { AsyncLocalStorageContextManager } from '@opentelemetry/context-async-hooks';


const provider = new BasicTracerProvider({
  resource: new Resource({
    [SEMRESATTRS_SERVICE_NAME]: 'github-actions'
  })
});
const exporter = new JaegerExporter({
  endpoint: 'http://localhost:14268/api/traces'
});

const contextManager = new AsyncLocalStorageContextManager();
contextManager.enable();
api.context.setGlobalContextManager(contextManager);


provider.addSpanProcessor(new SimpleSpanProcessor(new ConsoleSpanExporter()));
provider.addSpanProcessor(new SimpleSpanProcessor(exporter));
provider.register();
