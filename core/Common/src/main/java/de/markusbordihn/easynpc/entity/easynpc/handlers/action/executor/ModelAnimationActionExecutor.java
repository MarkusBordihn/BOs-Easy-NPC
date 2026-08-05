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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.api.animation.ModelAnimationAPI;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ModelAnimationActionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;

public final class ModelAnimationActionExecutor {

  private ModelAnimationActionExecutor() {}

  public static boolean play(ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC) {
    ModelAnimationActionData animationData = actionDataEntry.modelAnimationActionData();
    return ModelAnimationAPI.playAnimation(
        easyNPC,
        animationData.animationName(),
        animationData.playbackMode(),
        animationData.transition());
  }

  public static boolean stop(ActionDataEntry actionDataEntry, EasyNPC<?> easyNPC) {
    return ModelAnimationAPI.stopAnimation(
        easyNPC, actionDataEntry.modelAnimationActionData().transition());
  }

  public static boolean restart(EasyNPC<?> easyNPC) {
    return ModelAnimationAPI.restartAnimation(easyNPC);
  }
}
