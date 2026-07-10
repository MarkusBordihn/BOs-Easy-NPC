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

package de.markusbordihn.easynpc.client.renderer.entity.easymodelentities;

import de.markusbordihn.easymodelentities.api.client.EasyModelPartAnimator;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelPartTransform;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.EnumMap;
import java.util.Map;

/** Creates render-thread-safe snapshots of the fixed EasyNPC model-part pose. */
public final class EasyModelNPCPartAnimator {

  private EasyModelNPCPartAnimator() {}

  public static EasyModelPartAnimator snapshot(EasyModelNPC easyModelNPC) {
    if (easyModelNPC == null || !easyModelNPC.hasChangedModel()) {
      return EasyModelPartAnimator.NONE;
    }

    EnumMap<ModelPartType, EasyModelPartTransform> transforms =
        new EnumMap<>(ModelPartType.class);
    for (ModelPartType modelPartType : ModelPartType.values()) {
      if (modelPartType == ModelPartType.UNKNOWN) {
        continue;
      }
      CustomRotation rotation = easyModelNPC.getModelPartRotation(modelPartType);
      CustomPosition position = easyModelNPC.getModelPartPosition(modelPartType);
      CustomScale scale = easyModelNPC.getModelPartScale(modelPartType);
      transforms.put(
          modelPartType,
          new EasyModelPartTransform(rotation.x(), rotation.y(), rotation.z())
              .withOffset(position.x(), position.y(), position.z())
              .withScale(scale.x(), scale.y(), scale.z())
              .withVisible(easyModelNPC.getModelPartVisibility(modelPartType)));
    }

    return snapshot(transforms);
  }

  static EasyModelPartAnimator snapshot(Map<ModelPartType, EasyModelPartTransform> transforms) {
    if (transforms == null || transforms.isEmpty()) {
      return EasyModelPartAnimator.NONE;
    }
    Map<ModelPartType, EasyModelPartTransform> snapshot = Map.copyOf(transforms);
    return context -> {
      ModelPartType modelPartType = EasyModelEntitiesManager.getModelPartType(context.partName());
      return snapshot.getOrDefault(modelPartType, EasyModelPartTransform.NONE);
    };
  }
}
