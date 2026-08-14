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

package de.markusbordihn.easynpc.compat.easymodelentities;

import de.markusbordihn.easymodelentities.api.EasyModelApiContract;
import de.markusbordihn.easymodelentities.api.EasyModelEntitiesApi;
import de.markusbordihn.easymodelentities.api.EasyModelReloadEvents;
import de.markusbordihn.easymodelentities.api.client.EasyModelEntitiesClientApi;
import de.markusbordihn.easymodelentities.api.data.EasyModelBodyType;
import de.markusbordihn.easymodelentities.api.data.EasyModelProfileInfo;
import de.markusbordihn.easymodelentities.api.data.EasyModelProfileType;
import de.markusbordihn.easymodelentities.api.data.EasyModelTextureBlend;
import de.markusbordihn.easymodelentities.api.data.EasyModelTextureSetting;
import de.markusbordihn.easymodelentities.api.data.EasyModelTextureSlot;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.animation.ModelAnimationInfo;
import de.markusbordihn.easynpc.compat.IntegrationModelProvider;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.data.render.ModelTextureBlend;
import de.markusbordihn.easynpc.data.render.ModelTextureSetting;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyModelEntitiesLoader implements IntegrationModelProvider {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final EasyModelEntitiesLoader INSTANCE = new EasyModelEntitiesLoader();
  private static final int REQUIRED_API_MAJOR_VERSION = 2;
  private static final int REQUIRED_API_MINOR_VERSION = 1;

  private static boolean reloadListenerRegistered = false;
  private static boolean clientReloadListenerRegistered = false;

  private volatile List<ResourceLocation> cachedModels;

  private EasyModelEntitiesLoader() {}

  public static boolean isSupportedApiVersion() {
    String apiVersion;
    try {
      apiVersion = EasyModelApiContract.apiVersion();
    } catch (Throwable throwable) {
      log.error(
          "Unable to read the Easy Model Entities API version, integration will be disabled!",
          throwable);
      return false;
    }

    if (matchesRequiredApiVersion(apiVersion)) {
      return true;
    }

    log.error(
        "Easy Model Entities API version {} is not supported, {}.{}.x or newer within major "
            + "version {} is required, integration will be disabled!",
        apiVersion,
        REQUIRED_API_MAJOR_VERSION,
        REQUIRED_API_MINOR_VERSION,
        REQUIRED_API_MAJOR_VERSION);
    return false;
  }

  private static boolean matchesRequiredApiVersion(String apiVersion) {
    if (apiVersion == null) {
      return false;
    }

    String[] versionParts = apiVersion.trim().split("\\.");
    if (versionParts.length < 2) {
      return false;
    }

    try {
      return Integer.parseInt(versionParts[0]) == REQUIRED_API_MAJOR_VERSION
          && Integer.parseInt(versionParts[1]) >= REQUIRED_API_MINOR_VERSION;
    } catch (NumberFormatException e) {
      return false;
    }
  }

  public static void registerProfileReloadListener() {
    if (reloadListenerRegistered) {
      return;
    }

    reloadListenerRegistered = true;
    EasyModelReloadEvents.onProfileReload(
        () -> {
          INSTANCE.cachedModels = loadProfileModels();
          log.info("Loaded {} Easy Model Entities profiles", INSTANCE.cachedModels.size());
          IntegrationRegistry.invalidate(EasyModelEntitiesManager.INTEGRATION_ID);
        });
  }

  public static void register() {
    registerProfileReloadListener();
    if (INSTANCE.cachedModels == null || INSTANCE.cachedModels.isEmpty()) {
      INSTANCE.cachedModels = loadProfileModels();
    }
    IntegrationRegistry.register(INSTANCE);
  }

  public static void registerClient() {
    register();
    loadRenderableModels();
    if (clientReloadListenerRegistered) {
      return;
    }

    clientReloadListenerRegistered = true;
    EasyModelEntitiesManager.setAnimationProvider(
        new EasyModelEntitiesManager.AnimationProvider() {
          @Override
          public List<ModelAnimationInfo> listAnimations(ResourceLocation profileId) {
            return EasyModelEntitiesClientApi.listAnimations(profileId).stream()
                .map(
                    info ->
                        new ModelAnimationInfo(
                            info.name(),
                            info.baseName(),
                            info.durationTicks(),
                            info.loop(),
                            info.frameCount(),
                            info.keyframeCount(),
                            info.animatedBoneCount()))
                .toList();
          }

          @Override
          public List<String> listAnimationVariants(ResourceLocation profileId, String baseName) {
            return EasyModelEntitiesClientApi.listAnimationVariants(profileId, baseName);
          }
        });
    EasyModelEntitiesManager.setTextureProvider(
        new EasyModelEntitiesManager.TextureProvider() {
          @Override
          public List<String> listTextureSlots(ResourceLocation profileId) {
            return EasyModelEntitiesClientApi.listTextureSlots(profileId);
          }

          @Override
          public List<ResourceLocation> listTextureVariants(
              ResourceLocation profileId, String slot) {
            return EasyModelEntitiesClientApi.listTextureVariants(profileId, slot);
          }
        });
    EasyModelReloadEvents.onRenderProfileReload(EasyModelEntitiesLoader::loadRenderableModels);
  }

  public static EasyModelTextureSetting toEasyModelTextureSetting(
      ModelTextureSetting textureSetting) {
    if (textureSetting == null || textureSetting.isEmpty()) {
      return EasyModelTextureSetting.EMPTY;
    }

    Map<String, EasyModelTextureSlot> slots = new LinkedHashMap<>();
    textureSetting
        .slots()
        .forEach(
            (slot, textureSlot) ->
                slots.put(
                    slot,
                    new EasyModelTextureSlot(
                        textureSlot.texture(), toEasyModelTextureBlend(textureSlot.blend()))));
    return new EasyModelTextureSetting(slots);
  }

  private static EasyModelTextureBlend toEasyModelTextureBlend(ModelTextureBlend blend) {
    return switch (blend) {
      case TRANSLUCENT -> EasyModelTextureBlend.TRANSLUCENT;
      case CUTOUT -> EasyModelTextureBlend.CUTOUT;
    };
  }

  private static void loadRenderableModels() {
    INSTANCE.cachedModels = EasyModelEntitiesClientApi.listRenderableEntityProfileIds();
    EasyModelEntitiesManager.clearProfileModelMetadata();
    for (ResourceLocation profileId : INSTANCE.cachedModels) {
      EasyModelEntitiesClientApi.getBodyType(profileId)
          .ifPresent(bodyType -> registerProfileModelMetadata(profileId, bodyType));
    }
    IntegrationRegistry.invalidate(EasyModelEntitiesManager.INTEGRATION_ID);
  }

  private static List<ResourceLocation> loadProfileModels() {
    EasyModelEntitiesManager.clearProfileModelTypes();
    List<ResourceLocation> profileIds = new ArrayList<>();
    for (EasyModelProfileInfo profile : EasyModelEntitiesApi.listProfileInfos()) {
      if (profile.modelType() != EasyModelProfileType.ENTITY) {
        continue;
      }

      profileIds.add(profile.id());
      registerProfileModelMetadata(profile.id(), profile.bodyType());
      EasyModelEntitiesManager.registerProfileDimensions(
          profile.id(),
          profile.dimensions().width(),
          profile.dimensions().height(),
          profile.standingEyeHeight());
    }
    profileIds.sort(Comparator.comparing(ResourceLocation::toString));
    return profileIds;
  }

  private static void registerProfileModelMetadata(
      ResourceLocation profileId, EasyModelBodyType bodyType) {
    String bodyTypeName = bodyType.name();
    EasyModelEntitiesManager.registerProfileModelType(
        profileId, EasyModelEntitiesManager.getModelType(bodyTypeName));
    EasyModelEntitiesManager.registerProfileBodyType(profileId, bodyTypeName);
  }

  @Override
  public String getIntegrationId() {
    return EasyModelEntitiesManager.INTEGRATION_ID;
  }

  @Override
  public List<String> getAvailableModels() {
    List<ResourceLocation> models = this.cachedModels;
    if (models == null) {
      models = loadProfileModels();
      this.cachedModels = models;
      if (!models.isEmpty()) {
        log.debug("Loaded {} Easy Model Entities profiles", models.size());
      }
    }
    return models.stream().map(ResourceLocation::toString).toList();
  }
}
