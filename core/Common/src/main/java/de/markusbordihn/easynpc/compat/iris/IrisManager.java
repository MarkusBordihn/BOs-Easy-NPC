/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.compat.iris;

import de.markusbordihn.easynpc.Constants;
import java.lang.reflect.Method;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class IrisManager {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final List<String> API_CLASS_NAMES =
      List.of("net.irisshaders.iris.api.v0.IrisApi", "net.coderbot.iris.api.v0.IrisApi");
  private static final String PIPELINES_CLASS_NAME = "net.irisshaders.iris.pipeline.IrisPipelines";
  private static final String RENDER_PIPELINE_CLASS_NAME =
      "com.mojang.blaze3d.pipeline.RenderPipeline";

  private static Object apiInstance;
  private static Method shaderPackInUseMethod;
  private static boolean apiResolved;

  private IrisManager() {}

  public static boolean isShaderPackInUse() {
    if (!apiResolved) {
      resolveApi();
    }
    if (shaderPackInUseMethod == null) {
      return false;
    }

    try {
      return (boolean) shaderPackInUseMethod.invoke(apiInstance);
    } catch (ReflectiveOperationException | ClassCastException | LinkageError exception) {
      log.error(
          "Unable to read the Iris shader pack state, assuming no shader pack is in use: {}",
          exception.getMessage());
      shaderPackInUseMethod = null;
      return false;
    }
  }

  public static boolean copyShaderMapping(Object sourcePipeline, Object targetPipeline) {
    try {
      Class<?> renderPipelineClass = Class.forName(RENDER_PIPELINE_CLASS_NAME);
      Class.forName(PIPELINES_CLASS_NAME)
          .getMethod("copyPipeline", renderPipelineClass, renderPipelineClass)
          .invoke(null, sourcePipeline, targetPipeline);
      return true;
    } catch (ReflectiveOperationException exception) {
      log.debug("Unable to map a render pipeline for Iris: {}", exception.getMessage());
      return false;
    } catch (LinkageError error) {
      log.warn("Unable to link the Iris render pipeline mapping: {}", error.getMessage());
      return false;
    }
  }

  private static void resolveApi() {
    apiResolved = true;

    for (String apiClassName : API_CLASS_NAMES) {
      try {
        Class<?> apiClass = Class.forName(apiClassName);
        apiInstance = apiClass.getMethod("getInstance").invoke(null);
        shaderPackInUseMethod = apiClass.getMethod("isShaderPackInUse");
        log.info(
            "{} Iris shader pack detection over {} ...",
            Constants.LOG_REGISTER_PREFIX,
            apiClassName);
        return;
      } catch (ReflectiveOperationException exception) {
        log.debug("Iris API {} is not available: {}", apiClassName, exception.getMessage());
      } catch (LinkageError error) {
        log.warn(
            "Found a broken Iris or Oculus installation, {} could not be linked: {}",
            apiClassName,
            error.getMessage());
      }
    }
  }
}
