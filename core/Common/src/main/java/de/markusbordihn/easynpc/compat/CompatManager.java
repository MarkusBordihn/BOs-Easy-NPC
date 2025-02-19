/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.compat;

import de.markusbordihn.easynpc.Constants;

public class CompatManager {

  private static CompatHandlerInterface compatHandlerInterface;

  private CompatManager() {}

  public static void registerCompatHandler(CompatHandlerInterface compatHandlerInterface) {
    de.markusbordihn.easynpc.debug.Logger.INSTANCE.info("{} Compat Handler ...", Constants.LOG_REGISTER_PREFIX);
    CompatManager.compatHandlerInterface = compatHandlerInterface;
    compatHandlerInterface.register();
  }

  public static boolean isModLoaded(String modId) {
    if (compatHandlerInterface != null) {
      return compatHandlerInterface.isModLoaded(modId);
    }
    return false;
  }

  public static CompatHandlerInterface getHandler() {
    return compatHandlerInterface;
  }
}
