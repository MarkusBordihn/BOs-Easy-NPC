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

package de.markusbordihn.easynpc.client.renderer.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.model.CustomModelConfig;
import de.markusbordihn.easynpc.api.model.OriginalModelConfig;
import de.markusbordihn.easynpc.client.renderer.entity.state.EasyNPCRenderStateExtension;
import de.markusbordihn.easynpc.client.texture.LivingEntityTextureManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import java.util.function.Supplier;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.LivingEntity;

public interface EasyNPCEntityRenderer {

  Identifier getDefaultTexture();

  default boolean supportsPlayerSkins() {
    return false;
  }

  default OriginalModelConfig getOriginalModelConfig() {
    return OriginalModelConfig.DEFAULT;
  }

  default CustomModelConfig getCustomModelConfig() {
    return CustomModelConfig.NONE;
  }

  default Identifier getTransparentTexture() {
    return Constants.BLANK_ENTITY_TEXTURE;
  }

  default Identifier getTextureLocationWithConfig(final LivingEntity entity) {

    // Hide original model if custom model replaces it or if explicitly hidden
    OriginalModelConfig originalConfig = getOriginalModelConfig();
    if (getCustomModelConfig().shouldHideOriginal() || originalConfig.isHidden()) {
      return getTransparentTexture();
    }

    // Use custom texture from original model config if available
    if (originalConfig.hasCustomTexture()) {
      return originalConfig.getCustomTexture();
    }

    // Use EasyNPC skin system if entity is an EasyNPC
    if (entity instanceof EasyNPC<?> easyNPC) {
      return getEntityTexture(easyNPC);
    }

    // Fall back to default texture
    return getDefaultTexture();
  }

  default boolean hasEasyNPCRenderState(LivingEntityRenderState livingEntityRenderState) {
    return livingEntityRenderState instanceof EasyNPCRenderStateExtension;
  }

  default Identifier getTextureByVariant(final Enum<?> variant) {
    return LivingEntityTextureManager.getTextureByVariant(variant, getDefaultTexture());
  }

  default Identifier getCustomTexture(final SkinDataCapable<?> entity) {
    return LivingEntityTextureManager.getCustomTexture(entity, getDefaultTexture());
  }

  default Identifier getPlayerTexture(final SkinDataCapable<?> entity) {
    return LivingEntityTextureManager.getPlayerTexture(entity, getDefaultTexture());
  }

  default Identifier getRemoteTexture(final SkinDataCapable<?> entity) {
    return LivingEntityTextureManager.getRemoteTexture(entity, getDefaultTexture());
  }

  default EasyNPC<?> getEasyNPC(final LivingEntityRenderState livingEntityRenderState) {
    return EasyNPCLivingEntityRenderer.getEasyNPC(livingEntityRenderState);
  }

  default Identifier getTextureFromRenderState(final LivingEntityRenderState renderState) {
    return EasyNPCLivingEntityRenderer.getTexture(renderState, getDefaultTexture());
  }

  default Identifier getTextureFromRenderStateWithConfig(final LivingEntityRenderState renderState) {
    // Hide original model if custom model replaces it or if explicitly hidden
    OriginalModelConfig originalConfig = getOriginalModelConfig();
    if (getCustomModelConfig().shouldHideOriginal() || originalConfig.isHidden()) {
      return getTransparentTexture();
    }

    // Use custom texture from original model config if available
    if (originalConfig.hasCustomTexture()) {
      return originalConfig.getCustomTexture();
    }

    // Use EasyNPC skin system if render state contains EasyNPC data
    if (hasEasyNPCRenderState(renderState)) {
      return getTextureFromRenderState(renderState);
    }

    // Fall back to default texture
    return getDefaultTexture();
  }

  default Identifier getEntityTexture(final EasyNPC<?> easyNPC) {
    return LivingEntityTextureManager.getEntityTexture(easyNPC, getDefaultTexture());
  }

  default Identifier getEntityPlayerTexture(final EasyNPC<?> easyNPC) {
    return LivingEntityTextureManager.getEntityPlayerTexture(easyNPC, getDefaultTexture());
  }

  default Identifier getEntityTextureWithDefaultCallback(
      final EasyNPC<?> easyNPC, final Supplier<Identifier> defaultTextureSupplier) {
    return LivingEntityTextureManager.getEntityTextureWithDefaultCallback(
        easyNPC, getDefaultTexture(), defaultTextureSupplier);
  }
}
