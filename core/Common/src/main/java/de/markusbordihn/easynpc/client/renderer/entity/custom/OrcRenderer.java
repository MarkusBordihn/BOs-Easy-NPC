/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.entity.custom;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.custom.OrcModel;
import de.markusbordihn.easynpc.client.renderer.entity.EasyNPCEntityRenderer;
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Orc;
import de.markusbordihn.easynpc.entity.easynpc.npc.custom.Orc.Variant;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.Util;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.resources.ResourceLocation;

public class OrcRenderer<E extends Orc> extends HumanoidMobRenderer<E, OrcModel<E>>
    implements EasyNPCEntityRenderer {

  protected static final Map<Variant, ResourceLocation> TEXTURE_BY_VARIANT_TYPE =
      Util.make(
          new EnumMap<>(Variant.class),
          map -> {
            map.put(
                Variant.DEFAULT,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/orc/orc_default.png"));
            map.put(
                Variant.WARRIOR,
                new ResourceLocation(Constants.MOD_ID, "textures/entity/orc/orc_warrior.png"));
          });
  protected static final ResourceLocation DEFAULT_TEXTURE =
      TEXTURE_BY_VARIANT_TYPE.get(Variant.DEFAULT);

  public OrcRenderer(
      EntityRendererProvider.Context context, ModelLayerLocation modelLayerLocation) {
    super(context, new OrcModel<>(context.bakeLayer(modelLayerLocation)), 0.3F);
  }

  @Override
  public ResourceLocation getTextureLocation(E entity) {
    return getEntityTexture(entity);
  }

  public ResourceLocation getDefaultTexture() {
    return DEFAULT_TEXTURE;
  }

  public ResourceLocation getCustomTexture(SkinData<?> entity) {
    return CustomTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  public ResourceLocation getRemoteTexture(SkinData<?> entity) {
    return RemoteTextureManager.getOrCreateTextureWithDefault(entity, getDefaultTexture());
  }

  public ResourceLocation getTextureByVariant(Enum<?> variantType) {
    return TEXTURE_BY_VARIANT_TYPE != null
        ? TEXTURE_BY_VARIANT_TYPE.getOrDefault(variantType, DEFAULT_TEXTURE)
        : Constants.BLANK_ENTITY_TEXTURE;
  }

  public <N extends EasyNPC<?>> ResourceLocation getEntityTexture(N easyNPC) {
    SkinData<?> skinData = easyNPC.getEasyNPCSkinData();
    return switch (skinData.getSkinType()) {
      case NONE -> Constants.BLANK_ENTITY_TEXTURE;
      case CUSTOM -> getCustomTexture(skinData);
      case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> getRemoteTexture(skinData);
      default -> getTextureByVariant(easyNPC.getEasyNPCVariantData().getVariantType());
    };
  }
}
