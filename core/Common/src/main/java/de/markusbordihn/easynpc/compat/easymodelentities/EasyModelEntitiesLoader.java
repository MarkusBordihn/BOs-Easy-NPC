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

import de.markusbordihn.easymodelentities.api.EasyModelEntitiesApi;
import de.markusbordihn.easymodelentities.api.EasyModelReloadEvents;
import de.markusbordihn.easymodelentities.api.client.EasyModelEntitiesClientApi;
import de.markusbordihn.easymodelentities.api.data.EasyModelBodyType;
import de.markusbordihn.easymodelentities.api.data.EasyModelProfileInfo;
import de.markusbordihn.easymodelentities.api.data.EasyModelProfileType;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.api.animation.ModelAnimationInfo;
import de.markusbordihn.easynpc.compat.IntegrationModelProvider;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyModelEntitiesLoader implements IntegrationModelProvider {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final EasyModelEntitiesLoader INSTANCE = new EasyModelEntitiesLoader();

  private static boolean reloadListenerRegistered = false;
  private static boolean clientReloadListenerRegistered = false;

  private volatile List<Identifier> cachedModels;

  private EasyModelEntitiesLoader() {}

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
        profileId ->
            EasyModelEntitiesClientApi.listAnimations(profileId).stream()
                .map(
                    info ->
                        new ModelAnimationInfo(
                            info.name(),
                            info.durationTicks(),
                            info.loop(),
                            info.frameCount(),
                            info.keyframeCount(),
                            info.animatedBoneCount()))
                .toList());
    EasyModelReloadEvents.onRenderProfileReload(EasyModelEntitiesLoader::loadRenderableModels);
  }

  private static void loadRenderableModels() {
    INSTANCE.cachedModels = EasyModelEntitiesClientApi.listRenderableEntityProfileIds();
    EasyModelEntitiesManager.clearProfileModelMetadata();
    for (Identifier profileId : INSTANCE.cachedModels) {
      EasyModelEntitiesClientApi.getBodyType(profileId)
          .ifPresent(bodyType -> registerProfileModelMetadata(profileId, bodyType));
    }
    IntegrationRegistry.invalidate(EasyModelEntitiesManager.INTEGRATION_ID);
  }

  private static List<Identifier> loadProfileModels() {
    EasyModelEntitiesManager.clearProfileModelTypes();
    List<Identifier> profileIds = new ArrayList<>();
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
    profileIds.sort(Comparator.comparing(Identifier::toString));
    return profileIds;
  }

  private static void registerProfileModelMetadata(
      Identifier profileId, EasyModelBodyType bodyType) {
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
    List<Identifier> models = this.cachedModels;
    if (models == null) {
      models = loadProfileModels();
      this.cachedModels = models;
      if (!models.isEmpty()) {
        log.debug("Loaded {} Easy Model Entities profiles", models.size());
      }
    }
    return models.stream().map(Identifier::toString).toList();
  }
}
