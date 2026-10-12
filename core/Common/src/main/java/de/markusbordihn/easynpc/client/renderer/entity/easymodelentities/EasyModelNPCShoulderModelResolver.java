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

import de.markusbordihn.easymodelentities.api.data.client.EasyModelEntityRenderOptions;
import de.markusbordihn.easymodelentities.api.data.client.EasyModelShoulderModel;
import de.markusbordihn.easynpc.client.renderer.layer.ShoulderNPCCache;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.Optional;
import net.minecraft.nbt.CompoundTag;

public final class EasyModelNPCShoulderModelResolver {

  private EasyModelNPCShoulderModelResolver() {}

  public static Optional<EasyModelShoulderModel> resolve(CompoundTag entityTag) {
    return ShoulderNPCCache.get(entityTag)
        .filter(ShoulderNPCCache::isRenderedByEasyModelEntities)
        .map(EasyModelNPC.class::cast)
        .map(EasyModelNPCShoulderModelResolver::createShoulderModel);
  }

  private static EasyModelShoulderModel createShoulderModel(EasyModelNPC easyModelNPC) {
    EasyModelEntityRenderOptions renderOptions =
        AbstractEasyModelNPCRenderer.createRenderOptions(easyModelNPC, easyModelNPC);
    // EasyModelEntityRenderOptions#withScale is uniform, a non-uniform root scale keeps its height.
    CustomScale rootScale = easyModelNPC.getModelRootData().scale();
    if (rootScale.y() != 1.0F) {
      renderOptions = renderOptions.withScale(rootScale.y());
    }
    return new EasyModelShoulderModel(easyModelNPC.getEasyModelProfileId(), renderOptions);
  }
}
