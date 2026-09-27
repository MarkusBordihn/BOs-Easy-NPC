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

    OriginalModelConfig originalConfig = this.getOriginalModelConfig();
    if (this.getCustomModelConfig().shouldHideOriginal() || originalConfig.isHidden()) {
      return this.getTransparentTexture();
    }

    if (originalConfig.hasCustomTexture()) {
      return originalConfig.getCustomTexture();
    }

    if (entity instanceof EasyNPC<?> easyNPC) {
      return this.getEntityTexture(easyNPC);
    }

    return this.getDefaultTexture();
  }

  default boolean hasEasyNPCRenderState(LivingEntityRenderState livingEntityRenderState) {
    return livingEntityRenderState instanceof EasyNPCRenderStateExtension;
  }

  default Identifier getTextureByVariant(final Enum<?> variant) {
    return LivingEntityTextureManager.getTextureByVariant(variant, this.getDefaultTexture());
  }

  default Identifier getCustomTexture(final SkinDataCapable<?> entity) {
    return LivingEntityTextureManager.getCustomTexture(entity, this.getDefaultTexture());
  }

  default Identifier getPlayerTexture(final SkinDataCapable<?> entity) {
    return LivingEntityTextureManager.getPlayerTexture(entity, this.getDefaultTexture());
  }

  default Identifier getRemoteTexture(final SkinDataCapable<?> entity) {
    return LivingEntityTextureManager.getRemoteTexture(entity, this.getDefaultTexture());
  }

  default EasyNPC<?> getEasyNPC(final LivingEntityRenderState livingEntityRenderState) {
    return EasyNPCLivingEntityRenderer.getEasyNPC(livingEntityRenderState);
  }

  default Identifier getTextureFromRenderState(final LivingEntityRenderState renderState) {
    return EasyNPCLivingEntityRenderer.getTexture(renderState, this.getDefaultTexture());
  }

  default Identifier getTextureFromRenderStateWithConfig(
      final LivingEntityRenderState renderState) {
    OriginalModelConfig originalConfig = this.getOriginalModelConfig();
    if (this.getCustomModelConfig().shouldHideOriginal() || originalConfig.isHidden()) {
      return this.getTransparentTexture();
    }

    if (originalConfig.hasCustomTexture()) {
      return originalConfig.getCustomTexture();
    }

    if (this.hasEasyNPCRenderState(renderState)) {
      return this.getTextureFromRenderState(renderState);
    }

    return this.getDefaultTexture();
  }

  default Identifier getEntityTexture(final EasyNPC<?> easyNPC) {
    return LivingEntityTextureManager.getEntityTexture(easyNPC, this.getDefaultTexture());
  }

  default Identifier getEntityPlayerTexture(final EasyNPC<?> easyNPC) {
    return LivingEntityTextureManager.getEntityPlayerTexture(easyNPC, this.getDefaultTexture());
  }

  default Identifier getEntityTextureWithDefaultCallback(
      final EasyNPC<?> easyNPC, final Supplier<Identifier> defaultTextureSupplier) {
    return LivingEntityTextureManager.getEntityTextureWithDefaultCallback(
        easyNPC, this.getDefaultTexture(), defaultTextureSupplier);
  }
}
