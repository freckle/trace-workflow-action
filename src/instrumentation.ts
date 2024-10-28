/*instrumentation.ts*/
import * as api from '@opentelemetry/api';
import { Resource } from '@opentelemetry/resources';
import { BasicTracerProvider, BatchSpanProcessor, ConsoleSpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-node';
import { SEMRESATTRS_SERVICE_NAME } from '@opentelemetry/semantic-conventions';
import { JaegerExporter } from "@opentelemetry/exporter-jaeger";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { AsyncLocalStorageContextManager } from '@opentelemetry/context-async-hooks';

export function init(exporters: string[]) {
  const provider = new BasicTracerProvider({
    resource: new Resource({
      [SEMRESATTRS_SERVICE_NAME]: 'github-actions'
    })
  });

  const contextManager = new AsyncLocalStorageContextManager();
  contextManager.enable();
  api.context.setGlobalContextManager(contextManager);


  if (exporters.includes('jaeger')) {
    const exporter = new JaegerExporter({
      endpoint: 'http://localhost:14268/api/traces'
    });
    provider.addSpanProcessor(new SimpleSpanProcessor(exporter));
  }

  if (exporters.includes('jaeger')) {
    provider.addSpanProcessor(new SimpleSpanProcessor(new ConsoleSpanExporter()));
  }

  if (exporters.includes('collector')) {
    provider.addSpanProcessor(new BatchSpanProcessor(new OTLPTraceExporter()));
  }

  provider.register();
}
