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

package de.markusbordihn.easynpc.client.model;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.CustomAllayModel;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.event.EntityRenderersEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@OnlyIn(Dist.CLIENT)
public class ModModelLayers {

  // Model Layer Definitions
  public static final ModelLayerLocation ALLAY =
      new ModelLayerLocation(new ResourceLocation(Constants.MOD_ID, "allay"), "main");
  public static final ModelLayerLocation FAIRY =
      new ModelLayerLocation(new ResourceLocation(Constants.MOD_ID, "fairy"), "main");
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected ModModelLayers() {
  }

  public static void registerEntityLayerDefinitions(
      EntityRenderersEvent.RegisterLayerDefinitions event) {
    log.info("{} Entity Layer Definitions ...", Constants.LOG_REGISTER_PREFIX);

    event.registerLayerDefinition(ALLAY, CustomAllayModel::createBodyLayer);
    event.registerLayerDefinition(FAIRY, FairyModel::createBodyLayer);
  }
}
