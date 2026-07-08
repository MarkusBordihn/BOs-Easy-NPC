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
import de.markusbordihn.easymodelentities.data.profile.EasyModelEntityProfile;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.compat.IntegrationModelProvider;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.data.model.ModelType;
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

  private List<Identifier> cachedModels;

  private EasyModelEntitiesLoader() {}

  public static void register() {
    INSTANCE.cachedModels = loadProfileModels();
    if (!INSTANCE.cachedModels.isEmpty()) {
      log.info("Loaded {} Easy Model Entities profiles", INSTANCE.cachedModels.size());
    }
    IntegrationRegistry.register(INSTANCE);

    if (!reloadListenerRegistered) {
      reloadListenerRegistered = true;
      EasyModelReloadEvents.onProfileReload(
          () -> {
            INSTANCE.cachedModels = null;
            IntegrationRegistry.invalidate(EasyModelEntitiesManager.INTEGRATION_ID);
          });
    }
  }

  private static List<Identifier> loadProfileModels() {
    EasyModelEntitiesManager.clearProfileModelTypes();
    List<Identifier> profileIds = new ArrayList<>();
    for (EasyModelEntityProfile profile : EasyModelEntitiesApi.listProfiles()) {
      profileIds.add(profile.id());
      ModelType modelType = EasyModelEntitiesManager.getModelType(profile.bodyType().name());
      EasyModelEntitiesManager.registerProfileModelType(profile.id(), modelType);
    }
    profileIds.sort(Comparator.comparing(Identifier::toString));
    return profileIds;
  }

  @Override
  public String getIntegrationId() {
    return EasyModelEntitiesManager.INTEGRATION_ID;
  }

  @Override
  public List<String> getAvailableModels() {
    if (cachedModels == null || cachedModels.isEmpty()) {
      log.debug("Re-Loading Easy Model Entities profiles ...");
      List<Identifier> profileModels = loadProfileModels();
      if (!profileModels.isEmpty()) {
        this.cachedModels = profileModels;
        log.debug("Loaded {} Easy Model Entities profiles", cachedModels.size());
      }
    }

    if (cachedModels == null) {
      return List.of();
    }
    return cachedModels.stream().map(Identifier::toString).toList();
  }
}
